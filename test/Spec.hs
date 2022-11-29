-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec

import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (parseMaybe)
import qualified Text.Megaparsec as M

import Parser
import Program

import ParserSpec
import TypecheckerSpec

main :: IO ()
main = do
  ps <- parserSpec
  ts <- typecheckerSpec
  hspec $ (ps >> ts)
  -- TODO: Test the interpreter
  -- TODO: Test var scoping in if and while blocks
