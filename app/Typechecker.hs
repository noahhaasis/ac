{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Typechecker where

import Control.Monad.Except (MonadError, ExceptT, throwError, catchError, runExceptT, guard)
import Control.Monad.Reader (MonadReader, runReaderT, ReaderT, ask)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put, modify)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

import Program

data Success = Success
  deriving (Eq, Show)

data TypeError
  = UndefinedVariable Text
  | UndefinedFunction Text
  | TooManyArgs { functionName :: Text }
  | TooFewArgs { functionName :: Text }
  | ExpectedButGot Expr Type Type
  | WrongReturnType Statement Type Type
  deriving (Eq, Show)

data Environment = Environment (Map Text Type)

typecheck :: Program -> Either TypeError Success
typecheck p@(Program fns) = runReaderT typecheckResult (buildGlobalEnv p)
  where
    typecheckResult :: ReaderT GlobalEnv (Either TypeError) Success
    typecheckResult = (typecheckFunction `mapM` fns) >> return Success

typecheckFunction ::
  Function -> ReaderT GlobalEnv (Either TypeError) Success
typecheckFunction Function {..} = evalStateT res env
  where
    res :: StateT Environment (ReaderT GlobalEnv (Either TypeError)) Success
    res = (typecheckStatement returnType `mapM` body) >> return Success
    env = Environment (M.fromList parameters)

typecheckStatement :: 
  (MonadReader GlobalEnv m, MonadState Environment m, MonadError TypeError m) =>
  Type -> Statement -> m Success
typecheckStatement returnType (While {..}) = do
  typecheckExpr condition Boolean
  typecheckBlock returnType body
  return Success
typecheckStatement returnType (If {..}) = do
  typecheckExpr condition Boolean
  typecheckBlock returnType _then
  typecheckBlock returnType `mapM` _else
  return Success
typecheckStatement returnType (VarDecl {..}) = do
  typecheckExpr rhs _type
  modify (\(Environment env) -> Environment (M.insert name _type env))
  return Success
typecheckStatement returnType (Expr expr) = inferExpr expr >> return Success
typecheckStatement returnType s@(Return expr) =
  inferExpr expr >>= \returnType' -> if returnType' /= returnType
    then throwError $ WrongReturnType s returnType returnType'
    else return Success
typecheckStatement returnType (Assignment name expr) = do
  (Environment env) <- get
  case M.lookup name env of
    Just t -> typecheckExpr expr t
    Nothing -> throwError $ UndefinedVariable name

typecheckBlock :: 
  (MonadReader GlobalEnv m, MonadState Environment m, MonadError TypeError m) =>
  Type -> Block -> m Success
typecheckBlock returnType stmts = do
  env <- get
  typecheckStatement returnType `mapM` stmts
  put env
  return Success

typecheckExpr ::
  (MonadReader GlobalEnv m, MonadState Environment m, MonadError TypeError m) =>
  Expr -> Type -> m Success
typecheckExpr (Num _) Int = return Success
typecheckExpr (Bool _) Boolean = return Success
typecheckExpr (Character _) Char = return Success
typecheckExpr (EString _) String = return Success
typecheckExpr (BinaryOp Add l r) Int = typecheckExpr l Int >> typecheckExpr r Int
typecheckExpr (BinaryOp Sub l r) Int = typecheckExpr l Int >> typecheckExpr r Int
typecheckExpr (BinaryOp Mult l r) Int = typecheckExpr l Int >> typecheckExpr r Int
typecheckExpr (BinaryOp Lt l r) Boolean = typecheckExpr l Int >> typecheckExpr r Int
typecheckExpr e@(BinaryOp Equals l r) Boolean = do
  t1 <- inferExpr l
  t2 <- inferExpr r
  if t1 /= t2
    then throwError $ ExpectedButGot e t1 t2
    else return Success
typecheckExpr (BinaryOp Or l r) Boolean = typecheckExpr l Boolean >> typecheckExpr r Boolean
typecheckExpr e@(Var v) t = do
  (Environment env) <- get
  case M.lookup v env of
    Just t' | t == t' -> return $ Success
    Just t' -> throwError $ ExpectedButGot e t t'
    Nothing -> throwError $ UndefinedVariable v
typecheckExpr (Index a i) Char = typecheckExpr a String >> typecheckExpr i Int
typecheckExpr (FunctionCall "length" args) t = do
  case args of
    [str] -> do
      typecheckExpr str String
      return Success
    [] -> throwError $ TooFewArgs "length"
    _ -> throwError $ TooManyArgs "length"
typecheckExpr (FunctionCall "print" args) t = do
  case args of
    [arg] -> do
      -- Print can print values of any type
      _ <- inferExpr arg
      return Success
    [] -> throwError $ TooFewArgs "print"
    _ -> throwError $ TooManyArgs "print"
typecheckExpr (FunctionCall "read_line" args) t = do
  case args of
    [] -> return Success
    _ -> throwError $ TooManyArgs "read_line"
typecheckExpr e@(FunctionCall name args) t = do
  (GlobalEnv globalEnv) <- ask
  case M.lookup name globalEnv of
    Just (f@Function {..}) -> do
      if returnType /= t
        then throwError $ ExpectedButGot e t returnType
        else checkArgs f args
    Nothing -> throwError $ UndefinedFunction name
typecheckExpr expr expectedType = do
  actualType <- inferExpr expr
  throwError $ ExpectedButGot expr expectedType actualType

inferExpr ::
  (MonadReader GlobalEnv m, MonadState Environment m, MonadError TypeError m) =>
  Expr -> m Type
inferExpr (Num _) = return Int
inferExpr (Bool _) = return Boolean
inferExpr (Character _) = return Char
inferExpr (EString _) = return String
inferExpr (BinaryOp Add l r) =
  typecheckExpr l Int >> typecheckExpr r Int >> return Int
inferExpr (BinaryOp Sub l r) = 
  typecheckExpr l Int >> typecheckExpr r Int >> return Int
inferExpr (BinaryOp Mult l r) =
  typecheckExpr l Int >> typecheckExpr r Int >> return Int
inferExpr (BinaryOp Lt l r) =
  typecheckExpr l Int >> typecheckExpr r Int >> return Boolean
inferExpr e@(BinaryOp Equals l r) = do
  t1 <- inferExpr l
  t2 <- inferExpr r
  if t1 == t2
    then return Boolean
    else throwError $ ExpectedButGot e t1 t2
inferExpr (BinaryOp Or l r) =
  typecheckExpr l Boolean >> typecheckExpr r Boolean >> return Boolean
inferExpr (Var v) = do
  (Environment env) <- get
  case M.lookup v env of
    Just t -> return t
    Nothing -> throwError $ UndefinedVariable v
inferExpr (Index a i) = 
  typecheckExpr a String >> typecheckExpr i Int >> return Char
inferExpr (FunctionCall "length" args) = do
  case args of
    [str] -> do
      typecheckExpr str String
      return Int
    [] -> throwError $ TooFewArgs "length"
    _ -> throwError $ TooManyArgs "length"
inferExpr (FunctionCall "print" args) = do
  case args of
    [arg] -> do
      -- Print can print values of any type
      _ <- inferExpr arg
      return Unit
    [] -> throwError $ TooFewArgs "print"
    _ -> throwError $ TooManyArgs "print"
inferExpr (FunctionCall "read_line" args) = do
  case args of
    [] -> return String
    _ -> throwError $ TooManyArgs "read_line"
inferExpr (FunctionCall name args) = do
  (GlobalEnv globalEnv) <- ask
  case M.lookup name globalEnv of
    Just f -> do 
      _ <- checkArgs f args
      return $ returnType f
    Nothing -> throwError $ UndefinedFunction name

checkArgs ::
  (MonadReader GlobalEnv m, MonadState Environment m, MonadError TypeError m) =>
  Function -> [Expr] -> m Success
checkArgs (Function {..}) args = do
  uncurry typecheckExpr `mapM` (args `zip` (snd <$> parameters)) >> return Success
  if length args > length parameters
    then throwError $ TooManyArgs name
    else if length args < length parameters
    then throwError $ TooFewArgs name
    else return Success
