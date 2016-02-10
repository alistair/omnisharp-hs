module Internal where


import           Control.Applicative
import qualified Data.ByteString.Char8 as C
import           Data.Maybe
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Data.Word             (Word16)

import           Neovim
import           Neovim.API.String

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
