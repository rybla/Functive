{-# LANGUAGE LambdaCase #-}

module Grammar
( Prgm(..)
, Stmt(..)
, Expr(..), ExprPrim(..)
, Type(..), TypePrim(..)
, Name
, syneqExpr, syneqType
, getPrimExprType
) where

import           Data.ByteString          as BS
import           Data.ByteString.Internal

------------------------------------------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------------------------------------------

-- prgm
newtype Prgm =
    Prgm [Stmt]   -- [stmt]
  deriving (Show)

-- stmt
data Stmt =
    Module     Name [Stmt]    -- Begin Module n . [stmt] End Module n .
  | Definition Name Type Expr -- Definition n : t := e .
  | Signature  Name Type      -- Signature n = t .
  | Assumption Name Type      -- Assumption n : t .
  deriving (Show)

-- e | f | ...
data Expr =
    ExprName Name           -- n
  | ExprPrim ExprPrim       -- p
  | ExprFunc Name Expr      -- (fun n -> e)
  | ExprRecu Name Name Expr -- (rec n of m -> e)
  | ExprAppl Expr Expr      -- (e f)
  deriving (Show)

-- p
data ExprPrim =
    ExprPrimInt Int
  | ExprPrimBool Bool
  | ExprPrimUnit
  deriving (Show)

-- t | s | ...
data Type =
    TypeName Name      -- n
  | TypePrim TypePrim  -- P
  | TypeFunc Type Type -- (t -> s)
  | TypeAppl Type Type -- (t s)
  | TypeProd Name Type -- (forall n, t)
  | TypeCons Type Expr -- (t e)
  deriving (Show)

-- P
data TypePrim =
    TypePrimInt
  | TypePrimBool
  | TypePrimUnit
  | TypePrimType
  deriving (Show)

-- n
type Name =
    ByteString

--

------------------------------------------------------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------------------------------------------------------

-- syntactical equality

syneqExpr :: Expr -> Expr -> Bool
syneqExpr e f = error "unimplemented"

syneqType :: Type -> Type -> Bool
syneqType t s = error "unimplemented"

-- primitive exprs and types

getPrimExprType :: ExprPrim -> TypePrim
getPrimExprType = \case
  ExprPrimInt  _ -> TypePrimInt
  ExprPrimBool _ -> TypePrimBool
  ExprPrimUnit   -> TypePrimUnit
