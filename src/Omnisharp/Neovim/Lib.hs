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
import qualified Data.ByteString.Char8 as C

import Neovim
import Neovim.API.String

startServerPlugin :: CommandArguments -> String -> Neovim r (Maybe S.Server) ()
startServerPlugin _ f = do
    serverM <- get
    s <- liftIO $ es serverM f
    put $ Just s
    where
      es s f = case s of Just a  -> return a
                         Nothing -> S.startServer 8082 f


stopServerPlugin :: CommandArguments -> Neovim r (Maybe S.Server) ()
stopServerPlugin _ = do
    serverM <- get
    liftIO $ case serverM of
                Just a  -> S.stopServer a
                Nothing -> return ()
    put Nothing

getStatusPlugin :: Neovim r (Maybe S.Server) Bool
getStatusPlugin = do
    serverM <- get
    liftIO $ case serverM of
                (Just a)    -> S.getAliveStatus a
                Nothing -> return False

getTypePlugin :: CommandArguments -> Neovim r (Maybe S.Server) ()
getTypePlugin cargs = do
    serverM <- get
    (l,c) <- getCurrentPosition
    buffer <- getCurrentBuffer
    linesNum <- getNumberOfLines buffer
    text <- getLines buffer linesNum
    cwd <- getWorkingDirectory
    filepath <- getBufferName buffer
    req <- S.CodeActionRequest
              <$> return text
              <*> return ( (read . show) l)
              <*> return ((read . show) c)
              <*> return filepath
              <*> return False
    (S.TypeLookupResponse t d) <- liftIO $ case serverM of
                                          (Just server) -> S.lookupType server req
    case t of
      Just a  -> vim_command ("echo \"" ++ (T.unpack a) ++ "\"") >> return ()
      Nothing ->  vim_command ("echo \"Unable to determine type.\"") >> return ()


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

getCurrentBuffer :: Neovim r st Buffer
getCurrentBuffer = do
    buffer <- getBuffer
    return $ case buffer of Right a -> a
                            Left e -> Buffer (C.pack "")

    where
      getBuffer = vim_get_current_window >>= window_get_buffer

getCurrentPosition :: Neovim r st (Int64, Int64)
getCurrentPosition = extract <$> positionM
    where
      positionM = vim_get_current_window >>= window_get_cursor
      extract x = case x of (Right p)   -> p
                            (Left _)    -> (0, 0)

getWorkingDirectory :: Neovim r st String
getWorkingDirectory = do
    (Right d) <- vim_call_function "getcwd" []
    case fromObject d of Right a -> return a

-- Interestingly buffer_get_name seems to get the full path of the file....
getBufferName :: Buffer -> Neovim r st String
getBufferName b = do
    (Right n) <- buffer_get_name b
    return n

