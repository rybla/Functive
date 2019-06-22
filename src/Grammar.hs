module Grammar
( Program
) where

import           Data.ByteString          as BS
import           Data.ByteString.Internal

-- prgm
newtype Program =
    Program [Statement] -- [stmt]
  deriving (Show)

-- stmt
data Statement =
    Module     [Statement]    -- Begin Module n . [stmt] End Module n .
  | Definition Name Type Expr -- Definition n : t := e .
  | Signature  Name Type      -- Signature n = t .
  | Assumption Name Type      -- Assumption n : t .
  deriving (Show)

-- e | f | ...
data Expr =
    ExprName Name           -- n
  | ExprPrim ExprPrim       -- p
  | ExprApp  Expr Expr      -- (e f)
  | ExprFun  Name Expr      -- (fun n -> e)
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
  | TypeFun  Type Type -- (t -> s)
  | TypeApp  Type Type -- (t s)
  | TypeCon  Type Expr -- (t e)
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
