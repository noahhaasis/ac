{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import System.Environment
import Parser
import Interpreter

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename
  case parse $ T.pack content of
    Right program -> run program >> return ()
    Left error -> print error

