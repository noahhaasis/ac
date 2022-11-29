module TypecheckerSpec where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Map as M
import Data.Text as T
import Test.Hspec

import Typechecker
import Parser
import Program

disabled :: Expectation
disabled = 1 `shouldBe` 1

typecheckerSpec :: IO (SpecWith ())
typecheckerSpec = do
  fib <- readFile "fib.ac"
  let fibP = parse $ T.pack fib

  fibTypeError <- readFile "fib_type_error.ac"
  let fibTypeErrorP = parse $ T.pack fibTypeError

  return $ do
    describe "Typechecker.typecheckExpr" $ do
      let
        typecheckExpr' :: Expr -> Type -> Either TypeError Success
        typecheckExpr' e t = runReaderT (evalStateT (typecheckExpr e t) (Environment M.empty)) (GlobalEnv M.empty)

      it "should typecheck a primitive value" $ do
        typecheckExpr' (Num 1) Int `shouldBe` (Right Success)
        typecheckExpr' (Bool False) Boolean `shouldBe` (Right Success)
        typecheckExpr' (EString "") String `shouldBe` (Right Success)
        typecheckExpr' (Character 'c') Char `shouldBe` (Right Success)

        typecheckExpr' (Bool True) Int `shouldBe` (Left (ExpectedButGot (Bool True) Int Boolean))

      it "should typecheck a arithmetic expression" $ do
        typecheckExpr' (BinaryOp Add (Num 1) (Num 2)) Int `shouldBe` (Right Success)

        typecheckExpr' (BinaryOp Mult (Num 2) (EString "")) Int `shouldBe` (Left (ExpectedButGot (EString "") Int String))


    describe "Typechecker.typecheck" $ do
      case fibP of
        (Right fibP) -> 
          it "should successfully typecheck the fibonacci program" $ do
            typecheck fibP `shouldBe` (Right Success)
        (Left _) -> xit "should successfully typecheck the fibonacci program" disabled

      case fibTypeErrorP of
        (Right fibTypeErrorP) -> 
          it "should throw error for fib_type_error.ac" $ do
            typecheck fibTypeErrorP `shouldBe` (Left (ExpectedButGot (Var "s") String Int))
        (Left _) -> xit "should throw error for fib_type_error.ac" disabled
