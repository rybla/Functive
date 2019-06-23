{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Typing
( judgePrgm
) where

import           Control.Monad.Trans
import           Control.Monad.Trans.State as State
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.ByteString.Internal
import           Grammar

------------------------------------------------------------------------------------------------------------------------------
-- Judger
------------------------------------------------------------------------------------------------------------------------------

type Judger a = StateT Context JudgerStatus a

data JudgerStatus a = Consistent a | Inconsistent String

instance Functor JudgerStatus where
  fmap f ja = case ja of { Consistent a -> Consistent (f a) ; Inconsistent msg -> Inconsistent msg }

instance Applicative JudgerStatus where
  pure = Consistent
  jf <*> ja = case jf of { Consistent f -> fmap f ja ; Inconsistent msg -> Inconsistent msg }

instance Monad JudgerStatus where
  ja >>= a_jb = case ja of { Consistent a -> a_jb a ; Inconsistent msg -> Inconsistent msg }

instance Show a => Show (JudgerStatus a) where
  show (Consistent a)     = "Consistent " ++ show a
  show (Inconsistent msg) = "Inconsistent: " ++ msg

------------------------------------------------------------------------------------------------------------------------------
-- Type Context
------------------------------------------------------------------------------------------------------------------------------

-- types

data Context = Context
  { freeTypeVarCounter :: Int
  , rewrites           :: [Rewrite]
  , declarations       :: [Declaration]
  , children           :: [(Scope, Context)] }

type Scope       = ByteString

type Declaration = (Expr, TypeVar)
type Rewrite     = (FreeTypeVar, TypeVar)

data TypeVar     = Bound Type
                 | Free  FreeTypeVar
                 deriving (Show)

data FreeTypeVar = FreeName Name
                 | FreeTypeFunc TypeVar TypeVar
                 | FreeTypeAppl TypeVar TypeVar
                 | FreeTypeCons TypeVar Expr
                 | FreeTypeProd Name    TypeVar
                 deriving (Show)

-- syntactical equality

syneqFreeTypeVar :: FreeTypeVar -> FreeTypeVar -> Bool
syneqFreeTypeVar a b = error "unimplemented"

-- accessors and mutators

newFreeTypeVar :: Judger FreeTypeVar
newFreeTypeVar = do
  i <- gets freeTypeVarCounter
  modify $ \ctx -> ctx { freeTypeVarCounter=i+1 } -- increment
  return $ FreeName $ BS.concat [BSC.pack "t", BSC.pack . show $ i] -- create new FreeTypeVar

-- adds the declaration e:tv to the context
declare :: Expr -> TypeVar -> Judger ()
declare e tv =
  modify $ \ctx -> ctx { declarations=(e,tv):declarations ctx }

rewrite :: FreeTypeVar -> TypeVar -> Judger ()
rewrite a tv =
  modify $ \ctx -> ctx { rewrites=(a,tv):rewrites ctx }

-- apply any rewrites to the given typevar
getRewritten :: TypeVar -> Judger TypeVar
getRewritten (Bound t) = return (Bound t)
getRewritten (Free  a) =
  let helper mb' (b,s) =
        case mb' of
          Just b' -> Just b'
          Nothing -> if syneqFreeTypeVar a b then Just s else Nothing
  in do
    rs <- gets rewrites
    case foldl helper Nothing rs of
      Nothing  -> return (Free a)
      Just tv' -> getRewritten tv'

-- gets simplified type var declared for the given expression
-- if there is none in the current context, adds a new free typevar and returns that
getDeclaration :: Expr -> Judger TypeVar
getDeclaration e =
  let helper jma (f,b) =
        jma >>= \case
          Just a  -> return $ Just a
          Nothing -> if syneqExpr e f then Just <$> getRewritten b else return Nothing
  in do
    ds <- gets declarations
    mb_tv <- foldl helper (return Nothing) ds
    case mb_tv of
      Nothing -> do { a <- newFreeTypeVar ; declare e (Free a) ; getDeclaration e }
      Just tv -> return tv

-- attempts to unify the types of e and f in the context
-- may add rewrites to context
unify :: Expr -> Expr -> Judger ()
unify e f = do
  -- |- e:eT
  eT <- getDeclaration e
  -- |- f:fT
  fT <- getDeclaration f
  case (eT, fT) of
    (Bound t, Bound s) -> -- e:t , f:s
      if syneqType t s
        then return ()    -- f = s
        else lift $       -- f != s
          Inconsistent "unable to unify types"

    (Free  a, Bound s) -> -- e:a , f:s
      rewrite a (Bound s) -- => a := s

    (Bound t, Free  b) -> -- e:t , f:b
      rewrite b (Bound t) -- => b := t

    (Free  a, Free  b) -> -- e:a , f:b
      rewrite b (Free a) -- => b := a

------------------------------------------------------------------------------------------------------------------------------
-- Type Checking
------------------------------------------------------------------------------------------------------------------------------

judgePrgm :: Prgm -> Judger ()
judgePrgm (Prgm stmts) =
  foldl (>>) (return ()) (map judgeStmt stmts)

judgeStmt :: Stmt -> Judger ()
judgeStmt stmt =
  case stmt of
    Module n stmts ->
      error "unimplemented"
    Definition n t e -> do
      judgeExpr e
      declare (ExprName n) (Bound t)
      unify   (ExprName n) e
    Signature n t -> error "unimplemented"
    Assumption n t ->
      declare (ExprName n) (Bound t)

judgeExpr :: Expr -> Judger ()
judgeExpr expr = error "unimplemented"

judgeExprPrim :: ExprPrim -> Judger ()
judgeExprPrim prim = error "unimplemented"
