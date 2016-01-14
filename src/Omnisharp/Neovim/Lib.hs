{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Omnisharp.Neovim.Lib (
    startServerPlugin
   ,stopServerPlugin
   ,getStatusPlugin
) where

import qualified Omnisharp.Server as S

import Control.Applicative
import Data.Word (Word16)
import Data.Maybe
import qualified Data.Text as T

import Neovim

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

