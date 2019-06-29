{-# LANGUAGE LambdaCase #-}

module Typing
( checkPrgm, checkExpr
, TypeContext(..), emptyTypeContext
) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State as State
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import           Data.ByteString.Internal

import           Debug
import           Grammar
import           Reducing

------------------------------------------------------------------------------------------------------------------------------
-- Check
------------------------------------------------------------------------------------------------------------------------------

-- check

type Check a = StateT TypeContext IO a

-- handles check in temporary sub-state
subCheck :: Check a -> Check a
subCheck check = do{ saved_state <- get ; a <- check ; put saved_state ; return a }

------------------------------------------------------------------------------------------------------------------------------
-- Type Context
------------------------------------------------------------------------------------------------------------------------------

data TypeContext = TypeContext
  { freeTypeVarCounter :: Int
  , declarations       :: [Declaration]
  , rewrites           :: [Rewrite]
  , children           :: [(Scope, TypeContext)] }
type Scope       = ByteString
data Rewrite = Rewrite Name TypeVar
data Declaration = Declaration Expr TypeVar

instance Show TypeContext where
  show ctx =
    "rewrites:\n"     ++ foldl (\str r -> str++" - "++show r++"\n") "" (rewrites ctx) ++
    "declarations:\n" ++ foldl (\str d -> str++" - "++show d++"\n") "" (declarations ctx)

instance Show Declaration where
  show (Declaration e a) = show e++" : "++show a

instance Show Rewrite where
  show (Rewrite a b) = show a++" <- "++show b

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
  show (Bound t)      = show t
  show (FreeName n)   = show n
  show (FreeFunc a b) = "("++show a++" -> "++show b++")"
  show (FreeAppl a b) = "("++show a++" "++show b++")"
  show (FreeProd n a) = "(forall "++show n++", "++show a++")"
  show (FreeCons a e) = "("++show a++" "++show e++")"

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
  lift.inform $ "=> + "++show (FreeName . name $ "t"++show i)
  return . FreeName . name $ "t"++show i

getFreeNames :: TypeVar -> Check [Name]
getFreeNames = \case
  Bound    _ -> return []
  FreeName n -> return [n]
  FreeFunc a b -> do { fa <- getFreeNames a ; fb <- getFreeNames b ; return $ fa ++ fb }
  FreeAppl a b -> do { fa <- getFreeNames a ; fb <- getFreeNames b ; return $ fa ++ fb }
  FreeProd n a -> getFreeNames a
  FreeCons a e -> getFreeNames a

------------------------------------------------------------------------------------------------------------------------------
-- Type Unification
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------
-- rewriting

-- => « a <- t », where « a » is free and « t » cannot contain « a »
rewrite :: Rewrite -> Check ()
rewrite r@(Rewrite n t) = do
  lift.inform $ "=> "++show r
  tv <- getRewritten (FreeName n)
  t' <- getRewritten t
  case tv of
    fn@(FreeName n') -> do
      let r' = Rewrite n' t'
      fns <- getFreeNames t'
      if length fns > 1 && n' `elem` fns
        then fail $ "self-referencial rewrite: "++show r'
        else modify $ \ctx -> ctx { rewrites=r':rewrites ctx }

getRewrittenFreeName :: Name -> Check TypeVar
getRewrittenFreeName n = do
  let helper (Just n') _ = Just n' -- already found a rewrite
      helper Nothing (Rewrite n' s) = -- still looking for a rewrite
        if syneqName n n'
          then Nothing  -- self-referencial rewrite
          else Just n'  -- valid rewrite
  rs <- gets rewrites
  case foldl helper Nothing rs of
    Nothing -> return $ FreeName n
    Just n' -> getRewrittenFreeName n'

-- applies any rewrites in the given typevar
getRewritten :: TypeVar -> Check TypeVar
getRewritten = \case
    Bound b      -> return $ Bound b
    FreeName n   -> getRewrittenFreeName n
    FreeFunc a b -> do { a' <- getRewritten a ; b' <- getRewritten b ; return $ FreeFunc a' b' }
    FreeAppl a b -> do { a' <- getRewritten a ; b' <- getRewritten b ; return $ FreeAppl a' b' }
    FreeProd n a -> do { a' <- getRewritten a ; return $ FreeProd n a' }
    FreeCons a e -> do { a' <- getRewritten a ; return $ FreeCons a' e }

------------------------------------------------------------------------------------------------------------------------------
-- declaring

-- => « e : t »
declare :: Declaration -> Check ()
declare d@(Declaration e t) = do
  lift.inform $ "=> "++show d
  ds <- gets declarations
  let f (Declaration e' t') hasUnified = -- unify with the types of any previous declarations of « e »
        if syneqExpr e e'
          then unify t t' >> return True
          else return hasUnified
  hasUnified <- foldl (>>=) (return False) $ map f ds -- add new declaration if « e » has already been declared

  unless hasUnified $ modify $ \ctx -> ctx { declarations=d:ds }

-- gets simplified type var declared for the given expression
-- if there is none in the current context, errors
getDeclaration :: Expr -> Check TypeVar
getDeclaration e =
  -- scan for first matching declaration
  let helper (Just a) _                 = return $ Just a  -- already found match
      helper Nothing  (Declaration f t) = if syneqExpr e f -- check for match
                                            then Just <$> getRewritten t
                                            else return Nothing
  in do
    ds <- gets declarations
    mb_t <- foldl (\jma (Declaration f t) -> do { ma <- jma ; helper ma (Declaration f t) }) (return Nothing) ds
    case mb_t of
      Just t  -> return t
      Nothing -> fail $ "no declaration found for expr: " ++ show e

------------------------------------------------------------------------------------------------------------------------------
-- unification

-- attempts to unify types; may add rewrites to context
unify :: TypeVar -> TypeVar -> Check ()
unify tv1 tv2 = do
  lift.inform $ show tv1++" <~> "++show tv2
  let unableToUnify = fail $ "unable to unify bound types: " ++ show tv1 ++ " , " ++ show tv2
  let symmetric = unify tv2 tv1
  case (tv1, tv2) of
    -- Bound on left
    (Bound t,              FreeName n  )  -> rewrite $ Rewrite n (Bound t)
    (Bound t,              Bound s     )  -> unless (syneqType t s) unableToUnify
    (Bound (TypeFunc t s), FreeFunc a b)  -> do { unify (Bound t) a
                                                ; unify (Bound s) b }
    (Bound (TypeAppl t s), FreeAppl a b)  -> do { unify (Bound t) a
                                                ; unify (Bound s) b }
    (Bound (TypeProd n t), FreeProd m a)  -> do { nT <- getDeclaration (ExprName n)
                                                ; mT <- getDeclaration (ExprName m)
                                                ; unify nT mT
                                                ; unify (Bound t) a }
    (Bound (TypeCons t e), FreeCons a f)  -> do { unify (Bound t) a
                                                ; eT <- getDeclaration e
                                                ; fT <- getDeclaration f
                                                ; unify eT fT }
    -- Free on left
    (FreeName n,           a           )  -> rewrite $ Rewrite n a
    (FreeFunc a b,         FreeFunc c d)  -> do { unify a c
                                                ; unify b d }
    (FreeAppl a b,         FreeAppl c d)  -> do { unify a c
                                                ; unify b d }
    (FreeCons a e,         FreeCons b f)  -> do { unify a b
                                                ; eT <- getDeclaration e
                                                ; fT <- getDeclaration f
                                                ; unify eT fT }
    -- symmetric cases
    (FreeCons a e, _) -> symmetric
    (FreeFunc a b, _) -> symmetric
    (FreeAppl a b, _) -> symmetric

    -- no other pairs can unify
    (_,  _) -> unableToUnify

------------------------------------------------------------------------------------------------------------------------------
-- Type Checking
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------
-- check Prgm

checkPrgm :: Prgm -> Check ()
checkPrgm prgm@(Prgm stmts) = do
  lift.inform.show $ prgm
  foldl (>>) (return ()) (map checkStmt stmts)

------------------------------------------------------------------------------------------------------------------------------
-- check Stmt

checkStmt :: Stmt -> Check ()
checkStmt stmt = do
  lift.inform.show $ stmt
  case stmt of

    Definition n t e -> do
      declare $ Declaration (ExprName n) (Bound t) -- => n:t (global)
      checkExpr e; s <- getDeclaration e -- e:s
      unify (Bound t) s -- t <~> s

    Signature n t ->
      rewrite $ Rewrite n (Bound t) -- => n <- t (global)

    Assumption n t ->
      declare $ Declaration (ExprName n) (Bound t) -- => n:t (global)

  lift.inform $ "(Checked) "++show stmt

------------------------------------------------------------------------------------------------------------------------------
-- check Expr

checkExpr :: Expr -> Check ()
checkExpr expr = do
  lift.inform.show $ expr
  case expr of

    ExprName n -> do
      let ne = ExprName n
      a <- newFreeName -- + a
      declare $ Declaration ne a -- => n : a

    ExprPrim p -> do
      let pT = Bound . TypePrim $ getPrimExprType p
      declare $ Declaration (ExprPrim p) pT -- p : pT

    ExprFunc n e -> do
      let ne = ExprName n
      (a, b) <- subCheck $ do -- open local context
        { checkExpr e; b <- getDeclaration e -- e : b
        ; checkExpr ne; a <- getDeclaration ne -- n : a
        ; return (a, b) } -- close local context
      declare $ Declaration (ExprFunc n e) (FreeFunc a b) -- => (fun n => e) : (a -> b)

    ExprRecu n m e -> do
      let (ne, me) = (ExprName n, ExprName m)
      (a, b, c) <- subCheck $ do -- open local context
        { checkExpr e; b <- getDeclaration e -- e : b
        ; checkExpr me; a <- getDeclaration me -- n : c
        ; checkExpr ne; c <- getDeclaration ne -- m : a
        ; return (a, b, c) } -- close local context
      unify b c -- => b <~> c
      declare $ Declaration (ExprRecu n m e) (FreeFunc a b) -- (rec n of m => e) : a -> b

    ExprAppl e f -> do
      checkExpr e; c <- getDeclaration e -- e : c
      checkExpr f; a <- getDeclaration f -- f : a
      b <- newFreeName -- => + b
      unify c (FreeFunc a b) -- => c <~> (a -> b)
      declare $ Declaration (ExprAppl e f) b -- => (e f) : b

  t <- getDeclaration expr
  lift.inform $ "(Checked) "++show expr++" : "++show t
