{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Omnisharp.Server (
  Server,
  startServer,
  stopServer,
  getAliveStatus,
  lookupType,
  getCodeActions,
  CodeActionRequest (CodeActionRequest),
  TypeLookupResponse(..),
  CodeActionResponse(..)
  ) where

import           Control.Exception            (IOException,
                                               SomeException (SomeException),
                                               try)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString              as S
import           Data.ByteString.Lazy         (toStrict)
import           Data.Char
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Word                    (Word16)
import           System.IO.Streams            (InputStream, OutputStream,
                                               makeInputStream, stdout)
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec (ParseException)
import           System.IO.Streams.ByteString
import           System.IO.Temp
import           System.Process

import           Network.Http.Client          hiding (Port)
import           Omnisharp.Utils

omnisharp :: FilePath
omnisharp = "/home/alistair/Projects/YCM/omnisharp-roslyn/artifacts/build/omnisharp/omnisharp"

type Port = Word16

data Server = ServerProcess ProcessHandle Port

data CodeActionRequest = CodeActionRequest {
  buffer               :: T.Text,
  line                 :: Int,
  column               :: Int,
  filename             :: FilePath,
  includeDocumentation :: Bool
} deriving Show

instance ToJSON CodeActionRequest where
  toJSON (CodeActionRequest b l c f i) =
    object ["buffer" .= b, "column" .= show c, "filename" .= file, "includeDocumentation" .= i, "line" .= show l]
      where
        file = T.pack f

data TypeLookupResponse = TypeLookupResponse {
  codetype      :: Maybe T.Text,
  documentation :: Maybe T.Text
} deriving Show

instance FromJSON TypeLookupResponse where
  parseJSON (Object v)  = TypeLookupResponse <$>
                          v .: "Type" <*>
                          v .: "Documentation"
  parseJSON _           = mempty

data CodeActionResponse = CodeActionResponse {
  codeActions :: [T.Text]
} deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = toPascelCase} ''CodeActionResponse)

localhost :: S.ByteString
localhost = "127.0.0.1"

startServer :: Port -> FilePath -> IO Server
startServer p f = do
  (fp, handle) <- openTempFile "/tmp" "omnisharp-hs.log"
  (_, _, _, ph) <- createServer fp handle
  return $ ServerProcess ph p
    where
    createServer fp h = createProcess (proc omnisharp [ "-p", show p, "-s", f ])
                           { std_out = UseHandle h, std_err = UseHandle h}

stopServer :: Server -> IO ()
stopServer (ServerProcess _ port) =  withConnection conn (\c -> do
    q <- buildRequest $ http GET "/stopserver"

    sendRequest c q emptyBody
    r <- try $ receiveResponse c debugHandler
    case r of
      Right _ -> return ()
      Left (e :: ParseException) -> return () -- catch exception as server kills socket as its response. fun!
  )
  where
    conn = openConnection localhost port

getAliveStatus :: Server -> IO Bool
getAliveStatus (ServerProcess _ port) = withConnection conn (\c -> do
    q <- buildRequest $ http GET "/checkalivestatus"
    sendRequest c q emptyBody
    input <- receiveResponse c concatHandler
    return $ input == "true")
  where
    conn = openConnection localhost port

lookupType :: Server -> CodeActionRequest -> IO TypeLookupResponse
lookupType (ServerProcess _ port) req = withConnection conn (\c -> do
    q <- buildRequest $ http POST "/typelookup"
    body <- fromLazyByteString $ encode req
    sendRequest c q $ inputStreamBody body
    receiveResponse c jsonHandler :: IO TypeLookupResponse)
  where
    conn = openConnection localhost port


getCodeActions :: Server -> CodeActionRequest -> IO CodeActionResponse
getCodeActions (ServerProcess _ port) req = withConnection conn (\c -> do
    q <- buildRequest $ do
-- Here are some extra request configurations as an example
            http POST "/getcodeactions"
            setAccept "application/json"
            setHeader "X-Example" "A Value"
    body <- fromLazyByteString $ encode req
    sendRequest c q $ inputStreamBody body
    receiveResponse c jsonHandler :: IO CodeActionResponse)
  where
    conn = openConnection localhost port
