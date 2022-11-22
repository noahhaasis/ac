{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Program

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char

parse :: Text -> Either (ParseErrorBundle Text Void) Program
parse = M.parse programP ""

type Parser = Parsec Void Text

programP :: Parser Program
programP = Program <$> some (functionP <* space)

functionP :: Parser Function
functionP = do
    _ <- string "fun" >> space1
    name <- identifierP
    params <- parenthesized $ (paramP `sepBy` (char ',' >> space))
    _ <- space >> char ':' >> space
    returnType <- typeP
    _ <- space >> char '{' >> space
    body <- many (statementP <* space)
    _ <- space >> char '}'
    return $ Function name params returnType body
  where
    paramP = do
      name <- identifierP
      _ <- space >> char ':' >> space
      _type <- typeP
      return (name, _type)

statementP :: Parser Statement
statementP = whileP <|> ifP <|> returnP <|> varDeclP <|> try assignmentP <|> exprStatementP
  where
    varDeclP = do
      _ <- string "var" >> space1
      ident <- identifierP
      _ <- space >> char ':' >> space
      _type <- typeP
      _ <- space >> char '=' >> space
      rhs <- exprP
      _ <- char ';'
      return $ VarDecl ident _type rhs
    returnP = string "return" >> space1 >> (Return <$> exprP) <* char ';'
    whileP = do
      _ <- string "while" >> space
      cond <- parenthesized exprP
      _ <- space >> char '{' >> space
      body <- many (statementP <* space)
      _ <- space >> char '}'
      return $ While cond body
    ifP = do
      _ <- string "if" >> space
      cond <- parenthesized exprP
      _ <- space >> char '{' >> space
      _if <- many statementP
      _ <- space >> char '}'
      -- _then <- optional (space >> thenP)
      return $ If cond _if Nothing
    thenP = do
      _ <- string "else" >> space >> char '{' >> space
      _then <- many statementP
      _ <- space >> char '}'
      return _then 
    exprStatementP = (Expr <$> (exprP <* char ';'))
    assignmentP = do
      var <- identifierP
      _ <- space >> char '=' >> space
      rhs <- exprP
      _ <- space >> char ';'
      return $ Assignment var rhs

typeP :: Parser Type
typeP = (Int <$ string "int")
    <|> (String <$ string "string")
    <|> (Char <$ string "char")
    <|> (Unit <$ string "unit")

exprP :: Parser Expr
exprP = do
  term <- termP
  _ <- space
  binOp <- optional $ restOfBinaryOp term
  indexOp <- optional $ restOfIndexOp term
  return $ fromMaybe (fromMaybe term binOp) indexOp

termP :: Parser Expr
termP = parenthesized exprP <|> try valP <|> try functionCallP <|> varP
  where
    varP = Var <$> identifierP
    functionCallP = do
      name <- identifierP
      args <- parenthesized (exprP `sepBy` (string "," >> space))
      return $ FunctionCall name args

parenthesized :: Parser a -> Parser a
parenthesized = between (char '(') (char ')')

restOfBinaryOp :: Expr -> Parser Expr
restOfBinaryOp lhs = do
  op <- binaryOperatorP 
  _ <- space
  rhs <- exprP
  return $ BinaryOp op lhs rhs

restOfIndexOp :: Expr -> Parser Expr
restOfIndexOp operand = Index operand <$> between (char '[') (char ']') exprP

-- TODO: Predence
binaryOperatorP :: Parser BinaryOperator
binaryOperatorP
  =   (Add <$ string "+")
  <|> (Sub <$ string "-")
  <|> (Mult <$ string "*")
  <|> (Lt <$ string "<")
  <|> (Equals <$ string "==")
  <|> (Or <$ string "||")

identifierP :: Parser Text
identifierP = do
  ident <- (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
  return $ T.pack ident

valP :: Parser Expr
valP = charP <|> numP <|> boolP <|> stringP
  where
    charP = do -- TODO
      _ <- char '\''
      c <- alphaNumChar
      _ <- char '\''
      return $ Character c
    numP = (Num . read) <$> some digitChar
    boolP = (Bool True <$ string "true") <|> (Bool False <$ string "false")
    -- TODO
    stringP = do
      _ <- char '"'
      str <- many (alphaNumChar <|> spaceChar <|> char ':')
      _ <- char '"'
      return $ EString $ T.pack str
