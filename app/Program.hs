{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Program where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M

newtype Program = Program [Function]
  deriving (Eq, Show)

data Type = Unit | Int | String | Boolean | Char deriving (Eq, Show)

type Block = [Statement]

data GlobalEnv = GlobalEnv
  { functions :: Map Text Function
  }

buildGlobalEnv :: Program -> GlobalEnv
buildGlobalEnv (Program functions) =
  GlobalEnv (foldr (\f@Function {..} fs -> M.insert name f fs) M.empty functions)

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
  | EString Text
  | BinaryOp BinaryOperator Expr Expr
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
