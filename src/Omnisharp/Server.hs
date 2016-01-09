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
import Data.Maybe
import Data.Word (Word16)
import System.IO.Streams (InputStream, OutputStream, stdout)
import System.IO.Streams.Attoparsec(ParseException)
import qualified System.IO.Streams as Streams
import System.IO.Temp
import System.Process
import Control.Exception (try, IOException, SomeException(SomeException))
import Data.Typeable (typeOf)

import Network.Http.Client hiding (Port)

omnisharp :: FilePath
omnisharp = "/home/alistair/Projects/YCM/omnisharp-roslyn/artifacts/build/omnisharp/omnisharp"

type Port = Word16

data Server = ServerProcess ProcessHandle Port

--data CodeActionRequest = CodeActionRequest {
--    line :: Int,
--    column :: Int,
--    filename :: FilePath,
--    buffer :: S.ByteString
--  }

localhost = "127.0.0.1"

startServer :: Port -> FilePath -> IO Server
startServer p f = do
  (_, _, _, ph) <- withSystemTempFile "omnisharp-hs" createServer
  return $ ServerProcess ph p
    where
    createServer = (\fp h -> createProcess (proc omnisharp [ "-p", (show p), "-s", f ])
                                 { std_out = UseHandle h})

stopServer :: Server -> IO ()
stopServer (ServerProcess ph port) =  withConnection (conn) $ (\c -> do
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
getAliveStatus (ServerProcess ph port) = withConnection (conn) $ (\c -> do
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
