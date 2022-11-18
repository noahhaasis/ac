{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import System.Environment
import Parser

main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename
  print $ parse $ T.pack content
