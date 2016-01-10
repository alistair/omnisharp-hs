{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Omnisharp.Server (
  Server,
  startServer,
  stopServer,
  getAliveStatus
  ) where

import Control.Monad.IO.Class
import qualified Data.ByteString as S
import Data.ByteString.Lazy (toStrict)
import Data.Aeson
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Word (Word16)
import System.IO.Streams (InputStream, OutputStream, stdout, makeInputStream)
import System.IO.Streams.Attoparsec(ParseException)
import System.IO.Streams.ByteString
import qualified System.IO.Streams as Streams
import System.IO.Temp
import System.Process
import Control.Exception (try, IOException, SomeException(SomeException))
--import Data.Typeable (typeOf)

import Network.Http.Client hiding (Port)

omnisharp :: FilePath
omnisharp = "/home/alistair/Projects/YCM/omnisharp-roslyn/artifacts/build/omnisharp/omnisharp"

type Port = Word16

data Server = ServerProcess ProcessHandle Port

data CodeActionRequest = CodeActionRequest {
  buffer :: T.Text,
  column :: Int,
  filename :: FilePath,
  includeDocumentation :: Bool,
  line :: Int
} deriving Show

instance ToJSON CodeActionRequest where
  toJSON (CodeActionRequest b c f i l) =
    object ["buffer" .= b, "column" .= (show c), "filename" .= file, "includeDocumentation" .= i, "line" .= (show l)]
      where
        file = T.pack f

--  toEncoding (CodeActionRequest b c f i l) =
--    pairs ("buffer" .= b <> "column" .= c <> filename .= f <> "includeDocumentation" .= i <> "line" .= l)

data TypeLookupResponse = TypeLookupResponse {
  codetype :: Maybe T.Text,
  documentation :: Maybe T.Text
} deriving Show

instance FromJSON TypeLookupResponse where
  parseJSON (Object v)  = TypeLookupResponse <$>
                          v .: "Type" <*>
                          v .: "Documentation"
  parseJSON _           = mempty

localhost = "127.0.0.1"

startServer :: Port -> FilePath -> IO Server
startServer p f = do
  (_, _, _, ph) <- withSystemTempFile "omnisharp-hs" createServer
  return $ ServerProcess ph p
    where
    createServer = (\fp h -> createProcess (proc omnisharp [ "-p", (show p), "-s", f ])
                                 { std_out = UseHandle h})

stopServer :: Server -> IO ()
stopServer (ServerProcess _ port) =  withConnection (conn) $ (\c -> do
    q <- buildRequest $ do
      http GET "/stopserver"

    sendRequest c q emptyBody
    r <- try $ receiveResponse c debugHandler
    case r of
      Right _ -> return ()
      Left (e :: ParseException) -> return () -- catch exception as server kills socket as its response. fun!
  )
  where
    conn = openConnection localhost port

getAliveStatus :: Server -> IO Bool
getAliveStatus (ServerProcess _ port) = withConnection (conn) $ (\c -> do
    q <- buildRequest $ do
      http GET "/checkalivestatus"

    sendRequest c q emptyBody
    input <- (receiveResponse c concatHandler)
--    let r = case input of
--              Left e -> False
--              Right r -> r == "true"
    return $ input == "true")
  where
    conn = openConnection localhost port

lookupType :: Server -> CodeActionRequest -> IO TypeLookupResponse
lookupType (ServerProcess _ port) req = withConnection (conn) $ (\c -> do
    q <- buildRequest $ do
      http POST "/typelookup"
    body <- fromLazyByteString $ encode req
    sendRequest c q $ inputStreamBody body
    receiveResponse c jsonHandler :: IO TypeLookupResponse)
  where
    conn = openConnection localhost port
