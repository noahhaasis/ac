module Main where

import qualified Data.Text as T
import System.Environment
import Parser
import Interpreter
import Typechecker (typecheck, Success(..))

import Text.Megaparsec.Error


main :: IO ()
main = do
  [filename] <- getArgs
  content <- readFile filename
  case parse $ T.pack content of
    Right program -> do
      case typecheck program of
        Right Success -> run program >> return ()
        Left error -> print error
    Left error -> putStr $ errorBundlePretty error

