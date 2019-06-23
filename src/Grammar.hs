module Grammar
( Prgm(..)
, Stmt(..)
, Expr(..), ExprPrim(..)
, Type(..), TypePrim(..)
, Name
, syneqExpr, syneqType
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
  | ExprAppl Expr Expr      -- (e f)
  | ExprFunc Name Expr      -- (fun n -> e)
  | ExprRec  Name Name Expr -- (rec n of m -> e)
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
  | TypeCons Type Expr -- (t e)
  | TypeProd Name Type -- (forall n, t)
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
