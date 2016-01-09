module Main where

import Omnisharp.Server

main :: IO ()
main = do
  putStrLn "Starting Server"
  s <- startServer 2002 "/home/alistair/Projects/blackdunes.chronicles/src/blackdunes.chronicles"
  putStrLn "Getting status"
  status <- getAliveStatus s
  putStrLn $ show status
  stopServer s
