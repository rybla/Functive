module Reducing
( reduceExpr
, eqExpr
) where

import           Grammar

reduceExpr :: Expr -> Expr
reduceExpr e = error "unimplemented"

eqExpr :: Expr -> Expr -> Bool
eqExpr e f = syneqExpr (reduceExpr e) (reduceExpr f)
