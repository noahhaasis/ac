{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except (MonadError, ExceptT, throwError, catchError, runExceptT)
import Control.Monad.Reader (MonadReader, runReaderT, ReaderT, ask)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put, modify)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as M

import Program

data Value
  = VNum Int
  | VBool Bool
  | VChar Char
  | VString Text
  | VUnit
  deriving Show

data GlobalEnv = GlobalEnv
  { functions :: Map Text Function
  }

data Environment = Environment (Map Text Value)

emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty

evaluateFunction ::
  ( MonadReader GlobalEnv m
  , MonadState Environment m
  , MonadIO m
  )
  => Function -> [Value] -> m Value
evaluateFunction (Function _ params _ body) args = do
  (Environment env) <- get
  put $ Environment (M.fromList ((fst <$> params) `zip` args) `M.union` env)
  let res :: (MonadReader GlobalEnv m , MonadState Environment m , MonadIO m) => ExceptT Value m ()
      res = evaluateBlock body
  res' <- runExceptT res
  returnValue <- case res' of
    Left returnValue -> return returnValue
    Right () -> return VUnit
  put (Environment env)
  return returnValue

evaluateBlock :: 
  ( MonadReader GlobalEnv m
  , MonadState Environment m
  , MonadError Value m
  , MonadIO m
  )
  => Block -> m ()
evaluateBlock statements = const () <$> mapM evaluateStatement statements

evaluateStatement ::
  ( MonadReader GlobalEnv m
  , MonadState Environment m
  , MonadError Value m
  , MonadIO m
  )
  => Statement -> m ()
evaluateStatement whileStmt@(While {..}) = do
  cond <- evaluateExpr condition
  case cond of
    (VBool True) -> evaluateBlock body >> evaluateStatement whileStmt
    (VBool False) -> return ()
evaluateStatement (If {..}) = do
  cond <- evaluateExpr condition
  case cond of
    (VBool True) -> evaluateBlock _then
    (VBool False) -> case _else of
      Just _else -> evaluateBlock _else
      Nothing -> return ()
evaluateStatement (VarDecl {..}) = do
  val <- evaluateExpr rhs
  modify (\(Environment env) -> Environment (M.insert name val env))
evaluateStatement (Expr expr) = evaluateExpr expr >> return ()
evaluateStatement (Return expr) = do
  val <- evaluateExpr expr
  throwError val
evaluateStatement (Assignment name expr) = do
  val <- evaluateExpr expr
  modify (\(Environment env) -> Environment (M.insert name val env))

evaluateExpr ::
  ( MonadReader GlobalEnv m
  , MonadState Environment m
  , MonadIO m
  )
  => Expr -> m Value
evaluateExpr (Num num) = return $ VNum num
evaluateExpr (Bool bool) = return $ VBool bool
evaluateExpr (Character char) = return $ VChar char
evaluateExpr (EString str) = return $ VString str
evaluateExpr (BinaryOp op lhs rhs) = do
  l <- evaluateExpr lhs
  r <- evaluateExpr rhs
  case (op, l, r) of
    (Add, VNum a, VNum b) -> return $ VNum (a + b)
    (Sub, VNum a, VNum b) -> return $ VNum (a - b)
    (Mult, VNum a, VNum b) -> return $ VNum (a * b)
    (Lt, VNum a, VNum b) -> return $ VBool (a < b)
    (Equals, VNum a, VNum b) -> return $ VBool (a == b)
    (Equals, VChar a, VChar b) -> return $ VBool (a == b)
    (Or, VBool a, VBool b) -> return $ VBool (a || b)
    (op, l, r) -> error $ show (op, l, r)
evaluateExpr (Var v) = do
  (Environment env) <- get
  return $ fromJust $ M.lookup v env
evaluateExpr (Index {..}) = do
  arr <- evaluateExpr operand
  i <- evaluateExpr index
  case (arr, i) of
    (VString s, VNum i) -> return $ VChar (s `T.index` i)
evaluateExpr (FunctionCall "print" [arg]) = do
  val <- evaluateExpr arg
  case val of
    (VNum num) -> liftIO $ print num
    (VString str) -> liftIO $ putStrLn (T.unpack str)
  return VUnit
evaluateExpr (FunctionCall "read_line" []) = (VString . T.pack) <$> (liftIO getLine)
evaluateExpr (FunctionCall "length" [arg]) = do
  val <- evaluateExpr arg
  case val of
    VString str -> return $ VNum $ T.length str
evaluateExpr (FunctionCall fn argsE) = do
  GlobalEnv functions <- ask
  let (Just function) = M.lookup fn functions
  args <- mapM evaluateExpr argsE
  evaluateFunction function args

run :: Program -> IO Value
run (Program functions) = runReaderT (evalStateT mainResult emptyEnvironment) (GlobalEnv globalEnv)
  where
    mainResult :: StateT Environment (ReaderT GlobalEnv IO) Value
    mainResult = evaluateFunction mainFunction []
    globalEnv :: Map Text Function
    globalEnv = foldr (\f@Function {..} fs -> M.insert name f fs) M.empty functions
    mainFunction = fromJust $ M.lookup "main" globalEnv
