module Omnisharp.Utils (toPascelCase) where

import Data.Char

toPascelCase :: String -> String
toPascelCase [] = []
toPascelCase (x : xs) = toUpper x : xs

