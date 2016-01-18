{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Omnisharp.Neovim.Lib (
    startServerPlugin
   ,stopServerPlugin
   ,getStatusPlugin
   ,getTypePlugin
) where

import qualified Omnisharp.Server as S

import Control.Applicative
import Data.Word (Word16)
import Data.Maybe
import Data.Text.Encoding
import qualified Data.Text as T

import Neovim
import Neovim.API.String

startServerPlugin :: CommandArguments -> String -> Neovim r (Maybe S.Server) ()
startServerPlugin _ f = do
    serverM <- get
    s <- liftIO $ es serverM f
    put $ Just s
    where
      es s f = case s of (Just a)  -> return a
                         (Nothing) -> S.startServer 8082 f


stopServerPlugin :: CommandArguments -> Neovim r (Maybe S.Server) ()
stopServerPlugin _ = do
    serverM <- get
    liftIO $ case serverM of
                (Just a)  -> S.stopServer a
                (Nothing) -> return ()
    put Nothing

getStatusPlugin :: Neovim r (Maybe S.Server) Bool
getStatusPlugin = do
    serverM <- get
    liftIO $ case serverM of
                (Just a)    -> S.getAliveStatus a
                (Nothing) -> return False

getTypePlugin :: CommandArguments -> Neovim r (Maybe S.Server) ()
getTypePlugin cargs = do
    serverM <- get
    (l,c) <- getCurrentPosition
    buffer <- getCurrentBuffer
    req <- S.CodeActionRequest
              <$> return buffer
              <*> return ( (read . show) l)
              <*> return ((read . show) c)
              <*> return "/home/alistair/Projects/blackdunes.chronicles/src/blackdunes.chronicles/Startup.cs"
              <*> return False
    (S.TypeLookupResponse t d) <- liftIO $ case serverM of
                                          (Just server) -> S.lookupType server req
    case t of
      (Just a)  -> vim_out_write $ T.unpack a
      (Nothing) -> vim_out_write "Unable to determine type."


getLines :: Buffer -> Int64 -> Neovim t st T.Text
getLines b i = do
    lines <- sequence $ fmap (buffer_get_line b) [1..i]
    return (T.unlines $ makeString lines)
    where
      makeString l = case sequence l of Right a -> fmap T.pack a
                                        Left e -> []

getNumberOfLines :: Buffer -> Neovim t st Int64
getNumberOfLines b = do
    count <- buffer_line_count b
    let c = case count of Right a -> a
                          Left e -> 0
    return c

getCurrentBuffer :: Neovim r st T.Text
getCurrentBuffer = vim_get_current_window >>= window_get_buffer >>= convert
    where
      convert x = case x of Right r -> case r of Buffer b -> return $ decodeUtf8 b

getCurrentPosition :: Neovim r st (Int64, Int64)
getCurrentPosition = extract <$> positionM
    where
      positionM = vim_get_current_window >>= window_get_cursor
      extract x = case x of (Right p)   -> p
                            (Left _)    -> (0, 0)


