{-# LANGUAGE LambdaCase #-}

module Typing
( checkPrgm
, TypeContext(..), emptyTypeContext
, CheckStatus(..)
) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State as State
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.ByteString.Internal

import           Grammar
import           Reducing

------------------------------------------------------------------------------------------------------------------------------
-- Check
------------------------------------------------------------------------------------------------------------------------------

type Check a = StateT TypeContext CheckStatus a

data CheckStatus a = Ok a | Fail String

instance Functor CheckStatus where
  fmap f ja = case ja of { Ok a -> Ok (f a) ; Fail msg -> Fail msg }

instance Applicative CheckStatus where
  pure = Ok
  jf <*> ja = case jf of { Ok f -> fmap f ja ; Fail msg -> Fail msg }

instance Monad CheckStatus where
  ja >>= a_jb = case ja of { Ok a -> a_jb a ; Fail msg -> Fail msg }

instance Show a => Show (CheckStatus a) where
  show (Ok a)     = "Ok " ++ show a
  show (Fail msg) = "Fail: " ++ msg

-- handles check in temporary sub-state
subCheck :: Check a -> Check a
subCheck check = do{ saved_state <- get ; a <- check ; put saved_state ; return a }

------------------------------------------------------------------------------------------------------------------------------
-- Type Context
------------------------------------------------------------------------------------------------------------------------------

data TypeContext = TypeContext
  { freeTypeVarCounter :: Int
  , rewrites           :: [Rewrite]
  , declarations       :: [Declaration]
  , children           :: [(Scope, TypeContext)] }

type Scope       = ByteString

type Declaration = (Expr, TypeVar)
type Rewrite     = (TypeVar, TypeVar)

instance Show TypeContext where
  show ctx =
    "rewrites:\n"     ++ foldl (\str (t,s) -> str++" - "++show t++" := "++show s++"\n") "" (rewrites ctx) ++
    "declarations:\n" ++ foldl (\str (e,t) -> str++" - "++show e++" : " ++show t++"\n") "" (declarations ctx)

-- utilities

emptyTypeContext :: TypeContext
emptyTypeContext = TypeContext
  { freeTypeVarCounter = 0
  , rewrites           = []
  , declarations       = []
  , children           = [] }

------------------------------------------------------------------------------------------------------------------------------
-- Type Variable
------------------------------------------------------------------------------------------------------------------------------

data TypeVar     = Bound    Type
                 | FreeName Name
                 | FreeFunc TypeVar TypeVar
                 | FreeAppl TypeVar TypeVar
                 | FreeProd Name    TypeVar
                 | FreeCons TypeVar Expr

instance Show TypeVar where
  show (Bound t)    = show t
  show (FreeName n) = show n

-- syntactic equality

syneqTypeVar :: TypeVar -> TypeVar -> Bool
syneqTypeVar a b = case (a, b) of
  (Bound t, Bound s)           -> syneqType t s
  (FreeName n,   FreeName m  ) -> syneqName n m
  (FreeAppl t s, FreeAppl r q) -> syneqTypeVar t r && syneqTypeVar s q
  (FreeProd n t, FreeProd m s) -> syneqName    n m && syneqTypeVar t s
  (FreeCons t e, FreeCons s f) -> syneqTypeVar t s && eqExpr e f -- uses reduceExpr
  (_, _)                       -> False

-- utilities

-- create new FreeName of the form «t#» where "#" is a natural.
newFreeName :: Check TypeVar
newFreeName = do
  i <- gets freeTypeVarCounter
  modify $ \ctx -> ctx { freeTypeVarCounter=i+1 } -- increment
  return . FreeName . name $ "t"++show i

------------------------------------------------------------------------------------------------------------------------------
-- Type Unification
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------
-- declaring and rewriting

-- declare: « e:t »
declare :: Expr -> TypeVar -> Check ()
declare e t = modify $ \ctx -> ctx { declarations=(e,t):declarations ctx }

-- rewrite: « a:=t »
-- requires « a » is free
rewrite :: TypeVar -> TypeVar -> Check ()
rewrite (Bound s) t = fail $ "attempted to rewrite bound type: " ++ show s ++ ":=" ++ show t
rewrite a         t = modify $ \ctx -> ctx { rewrites=(a,t):rewrites ctx }

-- apply any rewrites to the given typevar
getRewritten :: TypeVar -> Check TypeVar
getRewritten (Bound t) = return (Bound t)
getRewritten a =
  -- scan for first matching rewrite
  let helper (Just b') (b,s) = Just b'             -- already found first matching
      helper Nothing   (b,s) = if syneqTypeVar a b -- check for match
        then Just s else Nothing
  in do
    rs <- gets rewrites
    case foldl helper Nothing rs of
      Nothing -> return a         -- no rewrite to apply, so return as is
      Just a' -> getRewritten a'  -- apply rewrite, then check for any others

-- gets simplified type var declared for the given expression
-- if there is none in the current context, errors
getDeclaration :: Expr -> Check TypeVar
getDeclaration e =
  -- scan for first matching declaration
  let helper (Just a) _     = return $ Just a  -- already found match
      helper Nothing  (f,t) = if syneqExpr e f -- check for match
        then Just <$> getRewritten t else return Nothing
  in do
    ds <- gets declarations
    mb_t <- foldl (\jma (f,t) -> do { ma <- jma ; helper ma (f,t) }) (return Nothing) ds
    case mb_t of
      Just t  -> return t
      Nothing -> fail $ "no declaration found for expr: " ++ show e

------------------------------------------------------------------------------------------------------------------------------
-- unification

-- attempts to unify types; may add rewrites to context
unify :: TypeVar -> TypeVar -> Check ()
unify tv1 tv2 =
  let unableToUnify = fail $ "unable to unify bound types: " ++ show tv1 ++ " , " ++ show tv2 in
  let symmetricCase = unify tv2 tv1 in
  case (tv1, tv2) of
    -- Bound on left
    (Bound t,              FreeName n  )  -> rewrite (FreeName n) (Bound t)
    (Bound t,              Bound s     )  -> unless (syneqType t s) unableToUnify
    (Bound (TypeFunc t s), FreeFunc a b)  -> do { unify (Bound t) a ; unify (Bound s) b }
    (Bound (TypeAppl t s), FreeAppl a b)  -> do { unify (Bound t) a ; unify (Bound s) b }
    (Bound (TypeProd n t), FreeProd m a)  -> do { nT <- getDeclaration (ExprName n) ; mT <- getDeclaration (ExprName m)
                                                ; unify nT mT
                                                ; unify (Bound t) a }
    (Bound (TypeCons t e), FreeCons a f)  -> do { unify (Bound t) a
                                                ; eT <- getDeclaration e ; fT <- getDeclaration f ; unify eT fT }
    -- Free on left
    (FreeFunc a b,         FreeFunc c d)  -> do { unify a c ; unify b d }
    (FreeAppl a b,         FreeAppl c d)  -> do { unify a c ; unify b d }
    (FreeCons a e,         FreeCons b f)  -> do { unify a b
                                                ; eT <- getDeclaration e ; fT <- getDeclaration f ; unify eT fT }
    -- symmetric cases
    (FreeName n,   _) -> symmetricCase
    (FreeCons a e, _) -> symmetricCase
    (FreeFunc a b, _) -> symmetricCase
    (FreeAppl a b, _) -> symmetricCase

    -- no other pairs can unify
    (_,  _) -> unableToUnify

------------------------------------------------------------------------------------------------------------------------------
-- Type Checking
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------
-- check Prgm

checkPrgm :: Prgm -> Check ()
checkPrgm (Prgm stmts) =
  foldl (>>) (return ()) (map checkStmt stmts)

------------------------------------------------------------------------------------------------------------------------------
-- check Stmt

checkStmt :: Stmt -> Check ()
checkStmt stmt =
  case stmt of
    Definition n t e -> do
      declare (ExprName n) (Bound t)      -- => n:t
      checkExpr e; s <- getDeclaration e  -- e:s
      unify (Bound t) s                   -- t ~ s

    Signature n t ->
      rewrite (FreeName n) (Bound t)      -- => n := t

    Assumption n t ->
      declare (ExprName n) (Bound t)      -- => n:t

------------------------------------------------------------------------------------------------------------------------------
-- check Expr

checkExpr :: Expr -> Check ()
checkExpr = \case
  ExprName n -> do
    let ne = ExprName n
    a <- newFreeName                                                        -- +a
    declare ne a                                                            -- => n:a

  ExprPrim p -> do
    let pT = Bound . TypePrim $ getPrimExprType p
    declare (ExprPrim p) pT                                                 -- p:pT

  ExprFunc n e -> do
    let ne = ExprName n
    (a, b) <- subCheck $ do                                                 -- open local context
      { checkExpr e; b <- getDeclaration e                                      -- e:b
      ; checkExpr ne; a <- getDeclaration ne                                    -- n:a
      ; return (a, b) }                                                     -- close local context
    declare (ExprFunc n e) (FreeFunc a b)                                   -- => (fun n => e):a->b

  ExprRecu n m e -> do
    let (ne, me) = (ExprName n, ExprName m)
    (a, b, c) <- subCheck $ do                                              -- open local context
      { checkExpr e; b <- getDeclaration e                                      -- e:b
      ; checkExpr me; a <- getDeclaration me                                    -- n:c
      ; checkExpr ne; c <- getDeclaration ne                                    -- m:a
      ; return (a, b, c) }                                                  -- close local context
    unify b c                                                               -- => b ~ c
    declare (ExprRecu n m e) (FreeFunc a b)                                 -- (rec n of m => e):a->b

  ExprAppl e f -> do
    checkExpr e; c <- getDeclaration e                                      -- e:c
    checkExpr f; a <- getDeclaration f                                      -- f:a
    b <- newFreeName                                                        -- => +b
    unify c (FreeFunc a b)                                                  -- => c ~ (a->b)
    declare (ExprAppl e f) b                                                -- => (e f):b
