{-# LANGUAGE LambdaCase #-}

module Grammar where

import           Data.ByteString          as BS
import           Data.ByteString.Char8    as BSC
import           Data.ByteString.Internal

import           Nat

------------------------------------------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------------------------------------------

-- prgm
newtype Prgm = Prgm [Stmt]
  deriving (Show)

-- stmt
data Stmt =
    Definition Name Type Expr -- Definition n : t := e .
  | Signature  Name Type      -- Signature n = t .
  | Assumption Name Type      -- Assumption n : t .
  deriving (Show)

-- e | f | ...
data Expr =
    ExprName Name           -- n
  | ExprFunc Name Expr      -- (fun n -> e)
  | ExprRecu Name Name Expr -- (rec n of m -> e)
  | ExprAppl Expr Expr      -- (e f)
  deriving (Eq)

instance Show Expr where
  show (ExprName n)     = show n
  show (ExprFunc n e)   = "(fun "++show n++" => "++show e++")"
  show (ExprRecu n m e) = "(rec "++show n++" of "++show m++" => "++show e++")"
  show (ExprAppl e f)   = "("++show e++" "++show f++")"

-- t | s | ...
data Type =
    TypeName Name      -- n
  | TypeFunc Type Type -- (t -> s)
  | TypeAppl Type Type -- (t s)
  | TypeProd Name Type -- (forall n, t)
  | TypeCons Type Expr -- (t e)
  deriving (Eq)

instance Show Type where
  show (TypeName n)   = show n
  show (TypeFunc t s) = show t++" -> "++show s
  show (TypeAppl t s) = "("++show t++" "++show s++")"
  show (TypeCons t e) = "("++show t++" "++show e++")"

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
