{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Program where

import Data.Text (Text)

newtype Program = Program [Function]
  deriving (Eq, Show)

data Type = Unit | Int | String | Char deriving (Eq, Show)

type Block = [Statement]

data Function
  = Function
    { name :: Text
    , parameters :: [(Text, Type)]
    , returnType :: Type
    , body :: Block
    }
  deriving (Eq, Show)

data Statement
  = While
    { condition :: Expr
    , body :: Block -- TODO: Include break
    }
  | If
    { condition :: Expr
    , _then :: Block
    , _else :: Maybe Block
    }
  | VarDecl
    { name :: Text
    , _type :: Type
    , rhs :: Expr
    }
  | Expr Expr
  | Return Expr
  | Assignment Text Expr
  deriving (Eq, Show)

data Expr
  = Num Int
  | Bool Bool
  | Character Char
  | BinaryOp BinaryOperator Expr Expr
  | Eq Expr Expr
  | Var Text
  | Index
    { operand :: Expr
    , index :: Expr
    }
  | FunctionCall
    { fun :: Text
    , args :: [Expr]
    }
  deriving (Eq, Show)

data BinaryOperator
  = Add
  | Sub
  | Mult
  | Lt
  | Equals
  | Or
  deriving (Eq, Show)
