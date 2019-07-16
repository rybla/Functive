{-# LANGUAGE LambdaCase #-}

module Grammar where

import           Nat

{-

  # Grammar Types

-}

-- prgm
newtype Prgm = Prgm [Stmt]

-- stmt
data Stmt =
    Definition Name Type Expr -- Definition n : t := e .
  | Fixedpoint Name Type Expr -- Fixedpoint n : t := e .
  | Signature  Name Type      -- Signature  n     := t .
  | Assumption Name Type      -- Assumption n : t .

-- e | f | ...
data Expr =
    ExprName Name           -- n
  | ExprFunc Name Expr      -- (fun n -> e)
  | ExprRecu Name Name Expr -- (rec n of m -> e)
  | ExprAppl Expr Expr      -- (e f)
  deriving (Eq)

-- t | s | ...
data Type =
    TypeName Name      -- n
  | TypeFunc Type Type -- (t -> s)
  | TypeAppl Type Type -- (t s)
  | TypeProd Name Type -- (forall n, t)
  | TypeCons Type Expr -- (t e)
  deriving (Eq)

-- n
newtype Name = Name String
  deriving (Eq)

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

{-

  ## Instances of Show

-}

instance Show Prgm where
  show (Prgm stmts) = foldl (\str stmt -> str++show stmt++" ") "" stmts

instance Show Stmt where
  show = \case
    Definition n t e -> "Definition "++show n++" : "++show t++" := "++show e++" ."
    Assumption n t   -> "Assumption "++show n++" : "++show t++" ."
    Signature  n t   -> "Signature "++show n++" := "++show t++" ."

instance Show Expr where
  show = \case
    ExprName n     -> show n
    ExprFunc n e   -> "("++show n++" => "++show e++")"
    ExprRecu n m e -> "(rec "++show n++" of "++show m++" => "++show e++")"
    ExprAppl e f   -> "("++show e++" "++show f++")"

instance Show Type where
  show = \case
    TypeName n   -> show n
    TypeFunc t s -> "("++show t++" -> "++show s++")"
    TypeAppl t s -> "("++show t++" "++show s++")"
    TypeProd n t -> "(forall "++show n++", "++show t++")"
    TypeCons t e -> "("++show t++" "++show e++")"

instance Show Name where
  show (Name n) = n
