module Typing
( judgePrgm
) where

import           Control.Monad.Trans
import           Control.Monad.Trans.State as State

import qualified Data.ByteString           as BS
import           Data.ByteString.Internal

import           Grammar

------------------------------------------------------------------------------------------------------------------------------
-- Judger
------------------------------------------------------------------------------------------------------------------------------

type Judger a = StateT Context JudgerStatus a
type JudgerStatus = Maybe -- TODO: make this into a pure logger that can report errors

------------------------------------------------------------------------------------------------------------------------------
-- Type Context
------------------------------------------------------------------------------------------------------------------------------

data Context = Context
  { freetypevars :: [FreeTypeVar]
  , rewrites     :: [Rewrite]
  , declarations :: [Declaration]
  , children     :: [(Scope, Context)] }

type Scope       = ByteString

type Declaration = (Expr, TypeVar)
type Rewrite     = (FreeTypeVar, TypeVar)

data TypeVar     = Bound Type
                 | Free  FreeTypeVar

data FreeTypeVar = FreeName Name
                 | FreeTypeFunc TypeVar TypeVar
                 | FreeTypeAppl TypeVar TypeVar
                 | FreeTypeCons TypeVar Expr
                 | FreeTypeProd Name    TypeVar

-- adds the declaration e:tv to the context
declare :: Expr -> TypeVar -> Judger ()
declare e tv =
  modify $ \ctx -> ctx { declarations=(e,tv):declarations ctx }

-- attempts to unify the types of e and f in the context
-- may add rewrites to context
unify :: Expr -> Expr -> Judger ()
unify e f = error "unimplemented"

-- gets simplified type var declared for the given expression
-- if there is none in the current context, adds a new free typevar and returns that
getDeclaration :: Expr -> Judger TypeVar
getDeclaration e = error "unimplemented"

------------------------------------------------------------------------------------------------------------------------------
-- Type Checking
------------------------------------------------------------------------------------------------------------------------------

judgePrgm :: Prgm -> Judger ()
judgePrgm (Prgm stmts) =
  foldl (>>) (return ()) (map judgeStmt stmts)

judgeStmt :: Stmt -> Judger ()
judgeStmt stmt =
  case stmt of
    Module n stmts   -> error "unimplemented"
    Definition n t e -> do
      { judgeExpr e
      ; declare (ExprName n) (Bound t)
      ; unify   (ExprName n) e }
    Signature n t -> error "unimplemented"
    Assumption n t ->
      declare (ExprName n) (Bound t)

judgeExpr :: Expr -> Judger ()
judgeExpr expr = error "unimplemented"

judgeExprPrim :: ExprPrim -> Judger ()
judgeExprPrim prim = error "unimplemented"
