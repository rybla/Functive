{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Typing
( judgePrgm
) where

import           Control.Monad
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
                 | FreeFunc TypeVar TypeVar
                 | FreeAppl TypeVar TypeVar
                 | FreeProd Name    TypeVar
                 | FreeCons TypeVar Expr
                 deriving (Show)

-- syntactical equality

syneqFreeTypeVar :: FreeTypeVar -> FreeTypeVar -> Bool
syneqFreeTypeVar a b = error "unimplemented"

-- accessors and mutators

newFreeName :: Judger FreeTypeVar
newFreeName = do
  i <- gets freeTypeVarCounter
  modify $ \ctx -> ctx { freeTypeVarCounter=i+1 } -- increment
  return . FreeName . BS.concat $ [BSC.pack "t", BSC.pack . show $ i] -- create new FreeTypeVar

-- adds the declaration e:tv to the context
declare :: Expr -> TypeVar -> Judger ()
declare e tv = modify $ \ctx -> ctx { declarations=(e,tv):declarations ctx }

rewrite :: FreeTypeVar -> TypeVar -> Judger ()
rewrite a tv = modify $ \ctx -> ctx { rewrites=(a,tv):rewrites ctx }

-- apply any rewrites to the given typevar
getRewritten :: TypeVar -> Judger TypeVar
getRewritten (Bound t) = return (Bound t)
getRewritten (Free  a) =
  -- scan for first matching rewrite
  let helper mb' (b,s) =
        case mb' of
          -- already found first matching
          Just b' -> Just b'
          -- check for match
          Nothing -> if syneqFreeTypeVar a b then Just s else Nothing
  in do
    rs <- gets rewrites
    case foldl helper Nothing rs of
      -- no rewrite to apply, so return as is
      Nothing  -> return (Free a)
      -- apply rewrite, then check for any others
      Just tv' -> getRewritten tv'

-- gets simplified type var declared for the given expression
-- if there is none in the current context, adds a new free typevar and returns that
getDeclaration :: Expr -> Judger TypeVar
getDeclaration e =
  -- scan for first matching declaration
  let helper jma (f,b) =
        jma >>= \case
          Just a  -> return $ Just a
          Nothing -> if syneqExpr e f then Just <$> getRewritten b else return Nothing
  in do
    ds <- gets declarations
    mb_tv <- foldl helper (return Nothing) ds
    case mb_tv of
      -- has been declared, so retrieve declared TypeVar
      Just tv -> return tv
      -- not declared yes, so make new FreeName for it
      Nothing -> do { a <- newFreeName ; declare e (Free a) ; getDeclaration e }

-- attempts to unify types; may add rewrites to context
unify :: TypeVar -> TypeVar -> Judger ()
unify tv tv' = case (tv, tv') of
  (Free  a, Free  b) -> rewrite b (Free a ) -- e:a , f:b  =>  b := a
  (Free  a, Bound s) -> rewrite a (Bound s) -- e:a , f:s  =>  a := s
  (Bound t, Free  b) -> rewrite b (Bound t) -- e:t , f:b  =>  b := t
  (Bound t, Bound s) -> if syneqType t s    -- e:t , f:s  =>  t = s
    then return () else lift $
      Inconsistent "unable to unify types"

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
      declare (ExprName n) (Bound t)      -- => n:t
      judgeExpr e; s <- getDeclaration e  -- e:s
      unify (Bound t) s                   -- t ~ s

    Signature n t ->
      rewrite (FreeName n) (Bound t)      -- => n := t

    Assumption n t ->
      declare (ExprName n) (Bound t)      -- => n:t

judgeExpr :: Expr -> Judger ()
judgeExpr = \case
  ExprName n ->
    void $ getDeclaration (ExprName n)

  ExprPrim p ->
    declare (ExprPrim p) (Bound . TypePrim $ getPrimExprType p)

  ExprFunc n e -> do
    judgeExpr e; b <- getDeclaration e          -- e:b
    a <- getDeclaration (ExprName n)            -- n:a
    declare (ExprFunc n e) (Free$ FreeFunc a b) -- (fun n => e):a->b

  ExprAppl e f -> do
    judgeExpr e; c <- getDeclaration e    -- e:c
    judgeExpr f; a <- getDeclaration f    -- f:a
    b <- newFreeName                      -- => +b
    unify c (Free $ FreeFunc a (Free b))  -- => c ~ (a->b)
    declare (ExprAppl e f) (Free b)       -- => (e f):b

  ExprRecu n m e -> do
    judgeExpr e; b <- getDeclaration e              -- e:b
    a <- getDeclaration (ExprName m)                -- m:a
    c <- getDeclaration (ExprName n)                -- n:c
    unify b c                                       -- => b ~ c
    declare (ExprRecu n m e) (Free $ FreeFunc a b)  -- (rec n of m => e):a->b
    return ()

judgeExprPrim :: ExprPrim -> Judger ()
judgeExprPrim prim = error "unimplemented"
