{-# LANGUAGE LambdaCase #-}

module Grammar
( Prgm(..)
, Stmt(..)
, Expr(..), ExprPrim(..)
, Type(..), TypePrim(..)
, Name(..), name
, syneqName, syneqExpr, syneqType
, getPrimExprType
) where

import           Data.ByteString          as BS
import           Data.ByteString.Char8    as BSC
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
  deriving (Eq)

instance Show Expr where
  show (ExprName n)     = show n
  show (ExprPrim p)     = show p
  show (ExprFunc n e)   = "(fun "++show n++" => "++show e++")"
  show (ExprRecu n m e) = "(rec "++show n++" of "++show m++" => "++show e++")"
  show (ExprAppl e f)   = "("++show e++" "++show f++")"

-- p
data ExprPrim =
    ExprPrimInt Int
  | ExprPrimBool Bool
  | ExprPrimUnit
  deriving (Eq)

instance Show ExprPrim where
  show (ExprPrimInt i)  = show i
  show (ExprPrimBool b) = show b
  show ExprPrimUnit     = "()"

-- t | s | ...
data Type =
    TypeName Name      -- n
  | TypePrim TypePrim  -- P
  | TypeFunc Type Type -- (t -> s)
  | TypeAppl Type Type -- (t s)
  | TypeProd Name Type -- (forall n, t)
  | TypeCons Type Expr -- (t e)
  deriving (Eq)

instance Show Type where
  show (TypeName n)   = show n
  show (TypePrim p)   = show p
  show (TypeFunc t s) = show t++" -> "++show s
  show (TypeAppl t s) = "("++show t++" "++show s++")"
  show (TypeCons t e) = "("++show t++" "++show e++")"

-- P
data TypePrim =
    TypePrimInt
  | TypePrimBool
  | TypePrimUnit
  | TypePrimType
  deriving (Eq)

instance Show TypePrim where
  show TypePrimInt  = "int"
  show TypePrimBool = "bool"
  show TypePrimUnit = "unit"
  show TypePrimType = "type"

-- n
newtype Name = Name ByteString
  deriving (Eq)

instance Show Name where
  show (Name bs) = BSC.unpack bs

name :: String -> Name
name = Name . BSC.pack

------------------------------------------------------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------------------------------------------------------

-- syntactical equality

syneqExpr :: Expr -> Expr -> Bool
syneqExpr = (==)

syneqType :: Type -> Type -> Bool
syneqType = (==)

syneqName :: Name -> Name -> Bool
syneqName = (==)

-- primitive exprs and types

getPrimExprType :: ExprPrim -> TypePrim
getPrimExprType = \case
  ExprPrimInt  _ -> TypePrimInt
  ExprPrimBool _ -> TypePrimBool
  ExprPrimUnit   -> TypePrimUnit
