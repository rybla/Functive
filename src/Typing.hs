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
  , rewrites           :: [Rewrite]
  , declarations       :: [Declaration]
  , children           :: [(Scope, TypeContext)] }
type Scope       = ByteString
data Declaration = Declaration Expr TypeVar
data Rewrite = Rewrite Name TypeVar

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
  lift . debug $ "+ "++show (FreeName . name $ "t"++show i)
  return . FreeName . name $ "t"++show i

freeNamesOf :: TypeVar -> Check [Name]
freeNamesOf = \case
  FreeName n -> return [n]
  FreeFunc a b -> do { fa <- freeNamesOf a ; fb <- freeNamesOf b ; return $ fa ++ fb }
  FreeAppl a b -> do { fa <- freeNamesOf a ; fb <- freeNamesOf b ; return $ fa ++ fb }
  FreeProd n a -> freeNamesOf a
  FreeCons a e -> freeNamesOf a

------------------------------------------------------------------------------------------------------------------------------
-- Type Unification
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------
-- reconciliation

-- goes through declarations and finds matching expressions, then unifies their types
reconcile :: Check ()
reconcile =
  let f :: [Declaration] -> Declaration -> Check ()
      f ds (Declaration e t) = foldl (>>) (return ()) $
                                  map (\(Declaration e' t') -> when (syneqExpr e e') $ unify t t')
                                    ds
  in do
    ds <- gets declarations
    foldl (>>) (return ()) $
      map (\i -> f (drop (i+1) ds) (ds !! i))
        [0..length ds - 1]

------------------------------------------------------------------------------------------------------------------------------
-- declaring and rewriting

-- declare: « e : t » where
declare :: Expr -> TypeVar -> Check ()
declare e t = do
  lift . debug . show $ Declaration e t
  modify $ \ctx -> ctx { declarations=Declaration e t:declarations ctx }
  reconcile

-- rewrite: « a <- t », where « a » is free
rewrite :: Name -> TypeVar -> Check ()
rewrite n t = do
  lift . debug . show $ Rewrite n t
  modify $ \ctx -> ctx { rewrites=Rewrite n t:rewrites ctx }

getRewrittenFreeName :: Name -> Check TypeVar
getRewrittenFreeName n =
  let recur ns n = -- keeps track of names used, preventing self-referencial rewrite
        let helper (Just n') _ = Just n' -- already found a rewrite
            helper Nothing (Rewrite n' s) = -- still looking for valid rewrite
              if syneqName n n' -- matching names
                then if n' `elem` ns  -- name has been used before?
                  then Nothing  -- self-referencial rewrite
                  else Just n'  -- valid rewrite
                else Nothing
        in do
          rs <- gets rewrites
          case foldl helper Nothing rs of
            Nothing -> return $ FreeName n
            Just n' -> recur (n:ns) n'
  in recur [] n

-- applies any rewrites in the given typevar
getRewritten :: TypeVar -> Check TypeVar
getRewritten = \case
    Bound b      -> return $ Bound b
    FreeName n   -> getRewrittenFreeName n
    FreeFunc a b -> do { a' <- getRewritten a ; b' <- getRewritten b ; return $ FreeFunc a' b' }
    FreeAppl a b -> do { a' <- getRewritten a ; b' <- getRewritten b ; return $ FreeAppl a' b' }
    FreeProd n a -> do { a' <- getRewritten a ; return $ FreeProd n a' }
    FreeCons a e -> do { a' <- getRewritten a ; return $ FreeCons a' e }

-- gets simplified type var declared for the given expression
-- if there is none in the current context, errors
getDeclaration :: Expr -> Check TypeVar
getDeclaration e =
  -- scan for first matching declaration
  let helper (Just a) _                 = return $ Just a  -- already found match
      helper Nothing  (Declaration f t) = if syneqExpr e f -- check for match
        then Just <$> getRewritten t else return Nothing
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
unify tv1 tv2 =
  (lift . debug $ show tv1++" <-> "++show tv2) >>
  let unableToUnify = fail $ "unable to unify bound types: " ++ show tv1 ++ " , " ++ show tv2 in
  let symmetric = unify tv2 tv1 in
  case (tv1, tv2) of
    -- Bound on left
    (Bound t,              FreeName n  )  -> rewrite n (Bound t)
    (Bound t,              Bound s     )  -> unless (syneqType t s) unableToUnify
    (Bound (TypeFunc t s), FreeFunc a b)  -> do { unify (Bound t) a ; unify (Bound s) b }
    (Bound (TypeAppl t s), FreeAppl a b)  -> do { unify (Bound t) a ; unify (Bound s) b }
    (Bound (TypeProd n t), FreeProd m a)  -> do { nT <- getDeclaration (ExprName n) ; mT <- getDeclaration (ExprName m)
                                                ; unify nT mT
                                                ; unify (Bound t) a }
    (Bound (TypeCons t e), FreeCons a f)  -> do { unify (Bound t) a
                                                ; eT <- getDeclaration e ; fT <- getDeclaration f ; unify eT fT }
    -- Free on left
    (FreeName n,           a           )  -> rewrite n a
    (FreeFunc a b,         FreeFunc c d)  -> do { unify a c ; unify b d }
    (FreeAppl a b,         FreeAppl c d)  -> do { unify a c ; unify b d }
    (FreeCons a e,         FreeCons b f)  -> do { unify a b
                                                ; eT <- getDeclaration e ; fT <- getDeclaration f ; unify eT fT }
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
checkPrgm (Prgm stmts) =
  foldl (>>) (return ()) (map checkStmt stmts)

------------------------------------------------------------------------------------------------------------------------------
-- check Stmt

checkStmt :: Stmt -> Check ()
checkStmt stmt = do
  lift . debug . show $ stmt
  case stmt of
    Definition n t e -> do
      declare (ExprName n) (Bound t)      -- => n:t (global)
      checkExpr e; s <- getDeclaration e  -- e:s
      unify (Bound t) s                   -- t <-> s

    Signature n t ->
      rewrite n (Bound t)                 -- => n <- t (global)

    Assumption n t ->
      declare (ExprName n) (Bound t)      -- => n:t (global)

------------------------------------------------------------------------------------------------------------------------------
-- check Expr

checkExpr :: Expr -> Check ()
checkExpr expr = (lift . debug . show $ expr) >> case expr of
  ExprName n -> do
    let ne = ExprName n
    a <- newFreeName -- +a
    declare ne a     -- => n:a

  ExprPrim p -> do
    let pT = Bound . TypePrim $ getPrimExprType p
    declare (ExprPrim p) pT                       -- p:pT

  ExprFunc n e -> do
    let ne = ExprName n
    (a, b) <- subCheck $ do                       -- open local context
      { checkExpr e; b <- getDeclaration e          -- e:b
      ; checkExpr ne; a <- getDeclaration ne        -- n:a
      ; return (a, b) }                           -- close local context
    declare (ExprFunc n e) (FreeFunc a b)         -- => (fun n => e):a->b

  ExprRecu n m e -> do
    let (ne, me) = (ExprName n, ExprName m)
    (a, b, c) <- subCheck $ do                    -- open local context
      { checkExpr e; b <- getDeclaration e          -- e:b
      ; checkExpr me; a <- getDeclaration me        -- n:c
      ; checkExpr ne; c <- getDeclaration ne      -- m:a
      ; return (a, b, c) }                        -- close local context
    unify b c                                     -- => b <-> c
    declare (ExprRecu n m e) (FreeFunc a b)       -- (rec n of m => e):a->b

  ExprAppl e f -> do
    checkExpr e; c <- getDeclaration e            -- e:c
    checkExpr f; a <- getDeclaration f            -- f:a
    b <- newFreeName                              -- => +b
    unify c (FreeFunc a b)                        -- => c <-> (a->b)
    declare (ExprAppl e f) b                      -- => (e f):b
