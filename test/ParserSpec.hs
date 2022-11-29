{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec

import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (parseMaybe)
import qualified Text.Megaparsec as M

import Parser
import Program

complexFunction :: Text
complexFunction = T.pack $ unlines
  [ "fun string_to_int(s: string): int {"
  , "  var result: int = 0;"
  , "  var i: int = 0;"
  , "  while (i < length(s)) {"
  , "    var c: char = si[i];"
  , "    result = result * 10;"
  , "    result = result + character_to_int(c);"
  , "    i = i + 1;"
  , "  }"
  , "  return result;"
  , "}"
  ]

complexWhileLoop :: Text
complexWhileLoop = T.pack $ unlines
  [ "while (i < length(s)) {"
  , "  var c: char = s[i];"
  , "  result = result * 10;"
  , "  result = result + character_to_int(c);"
  , "  i = i + 1;"
  , "}"
  ]

parsedComplexWhileLoop :: Statement
parsedComplexWhileLoop =
  While
    (BinaryOp Lt (Var "i") (FunctionCall "length" [Var "s"]))
    [ (VarDecl "c" Char (Index (Var "s") (Var "i")))
    , (Assignment "result" (BinaryOp Mult (Var "result") (Num 10)))
    , (Assignment "result" (BinaryOp Add (Var "result") (FunctionCall "character_to_int" [Var "c"])))
    , (Assignment "i" (BinaryOp Add (Var "i") (Num 1)))
    ]

parserSpec :: IO (SpecWith ())
parserSpec = do
  fibProgram <- T.pack <$> readFile "fib.ac"
  return $ do
    describe "Parser.exprP" $ do
      it "parses a simple number" $ do
        parseMaybe exprP "42" `shouldBe` Just (Num 42)
      it "parses the boolean value true" $ do
        parseMaybe exprP "true" `shouldBe` Just (Bool True)

      it "parses a single binary operation" $ do
        parseMaybe exprP "1 + 1" `shouldBe` Just (BinaryOp Add (Num 1) (Num 1))
        parseMaybe exprP "true || false" `shouldBe` Just (BinaryOp Or (Bool True) (Bool False))
        parseMaybe exprP "4 * 2" `shouldBe` Just (BinaryOp Mult (Num 4) (Num 2))

      it "parses indexing operation" $ do
        parseMaybe exprP "array[1]" `shouldBe` Just (Index (Var "array") (Num 1))

      it "parses a string literal" $ do
        parseMaybe exprP "\"test string expression\"" `shouldBe` Just (EString "test string expression")

      it "parses a parenthesized expression" $ do
        parseMaybe exprP "(1 + 2)" `shouldBe` Just (BinaryOp Add (Num 1) (Num 2))

      it "parses indexing operation with complex subexpressions" $ do
        parseMaybe exprP "(4 + 2)[1 + 2]" `shouldBe` Just (Index (BinaryOp Add (Num 4) (Num 2)) (BinaryOp Add (Num 1) (Num 2)))

      it "parses a function call" $ do
        parseMaybe exprP "min(1, 2)" `shouldBe` Just (FunctionCall "min" [Num 1, Num 2])

      it "parses a complex expression" $ do
        parseMaybe exprP "print(fib(string_to_int(read_line())))" `shouldBe` Just (FunctionCall "print" [FunctionCall "fib" [FunctionCall "string_to_int" [FunctionCall "read_line" []]]])

      xit "respects operator precedence" $ do
        parseMaybe exprP "n == 0 || n == 1" `shouldBe` Just (BinaryOp Or (BinaryOp Equals (Var "n") (Num 0)) (BinaryOp Equals (Var "n") (Num 1)))

    describe "Parser.statementP" $ do
      it "parses a return statement" $ do
        parseMaybe statementP "return true;" `shouldBe` Just (Return (Bool True))

      it "parses a minimal while statement" $ do
        parseMaybe statementP "while (true) { }" `shouldBe` Just (While (Bool True) [])

      it "parses a minimal if statement" $ do
        parseMaybe statementP "if (true) { }" `shouldBe` Just (If (Bool True) [] Nothing)

      -- TODO
      xit "parses a minimal if then else statement" $ do
        parseMaybe statementP "if (true) { } else { }" `shouldBe` Just (If (Bool True) [] Nothing)

      it "parses an assignment" $ do
        parseMaybe statementP "x = 1;" `shouldBe` Just (Assignment "x" (Num 1))

      it "parses a complex while loop" $ do
        M.parse statementP "" complexWhileLoop `shouldBe` Right parsedComplexWhileLoop
        -- TODO: Why doesn't this work?
        -- parseMaybe statementP complexWhileLoop `shouldBe` Just parsedComplexWhileLoop
        -- Answer: Because it wants to consume `eof`, so the whole input
      
      it "parses a minimal function" $ do
        parseMaybe functionP "fun f(): unit { }" `shouldBe` Just (Function "f" [] Unit [])

      it "parses a simple function" $ do
        parseMaybe functionP "fun id(n: int): int { return n; }" `shouldBe` Just (Function "id" [("n", Int)] Int [Return (Var "n")])

    describe "Parser.functionP" $ do
      it "parses a complex function" $ do
        M.parse functionP "" complexFunction `shouldSatisfy` isRight

    describe "Parser.parse" $ do
      it "parses a fibonacci program" $ do
        parse fibProgram `shouldSatisfy` isRight
    -- TODO: Test the interpreter


  -- TODO: Test var scoping in if and while blocks
