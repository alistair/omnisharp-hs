{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Omnisharp.Neovim.Plugin (plugin) where

import Neovim
import Omnisharp.Neovim.Lib (startServerPlugin, stopServerPlugin)
import Omnisharp.Server
import Data.Maybe

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = wrapPlugin Plugin
    { exports         = []
    , statefulExports =
      [ ((), Nothing,
        [
          $(command "OmnisharpStartServer" 'startServerPlugin) [CmdSync Sync],
          $(command "OmnisharpStopServer" 'stopServerPlugin) [CmdSync Sync]
        ])
      ]
    }



