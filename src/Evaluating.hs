{-# LANGUAGE LambdaCase #-}

module Evaluating where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State as State

import           Debug
import           Grammar

{-

  # Evaluation

-}

type Evaluation a = StateT EvaluationContext IO a

{-

  ## Evaluation Context

-}

data EvaluationContext = ReductionContext
  { reductions :: [Reduction] }

-- e => e'
data Reduction = Reduction Expr Expr

instance Show EvaluationContext where
  show ctx =
    "reductions:\n" ++ foldl (\str r -> str++" - "++show r++"\n") "" (reductions ctx)

instance Show Reduction where
  show (Reduction e e') = show e++" => "++show e'

{-

  ## Reducing Expressions

-}

reduce :: Reduction -> Evaluation ()
reduce r@(Reduction e e') = do
  rs <- gets reductions
  let f rs' (Reduction f f') = if syneqExpr e f then r:rs' else Reduction f f':rs'
  let rs' = foldl f [] rs
  modify $ \ctx -> ctx { reductions=r:reductions ctx }

getReduced :: Expr -> Evaluation Expr
getReduced e =
  let f Nothing  r@(Reduction f f') = if syneqExpr e f then Just r else Nothing
      f (Just r) _                  = Just r
  in do
    rs <- gets reductions
    case foldl f Nothing rs of
      Just (Reduction e e') -> getReduced e'
      Nothing               -> return e


{-

  ## Evaluate

-}

evaluate :: Expr -> Evaluation Expr
evaluate expr = do
  lift.debug $ "evaluate: "++show expr
  case expr of
    ExprName n                -> return $ ExprName n
    ExprFunc n e              -> return $ ExprFunc n e
    ExprRecu n m e            -> return $ ExprRecu n m e
    ExprAppl (ExprFunc n f) e -> substitute e n <$> getReduced f
    ExprAppl e f              -> return $ ExprAppl e f

-- [e/n]f
substitute :: Expr -> Name -> Expr -> Expr
substitute e n = \case
  ExprName n'       -> if syneqName n n' then e else ExprName n'
  ExprFunc n' e'    -> if syneqName n n' then ExprFunc n' e'
                                         else ExprFunc n' (substitute e n e')
  ExprRecu n' m' e' -> if syneqName n n'
                       || syneqName n m' then ExprRecu n' m' e
                                         else ExprRecu n' m' (substitute e n e')
  ExprAppl e' f'    -> ExprAppl (substitute e n e') (substitute e n f')

{-

  # Misc

-}

-- evaluateExpr :: Expr -> Expr
-- evaluateExpr e =

-- eqExpr :: Expr -> Expr -> Bool
-- eqExpr e f = syneqExpr (evaluateExpr e) (evaluateExpr f)
