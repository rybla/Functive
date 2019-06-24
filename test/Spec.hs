import           Control.Monad.Trans.State
import           Grammar
import           Typing

e = ExprName $ name "e"
f = ExprName $ name "f"

t = TypeName $ name "t"
n = name "n"

prgm = Prgm
  [ Definition n t (ExprAppl e f) ]



main :: IO ()
main = do
  putStrLn "\n==================================================================================================\n"

  case runStateT (checkPrgm prgm) emptyTypeContext of
    Consistent ((), ctx) -> print ctx
    Inconsistent msg     -> putStrLn msg

  putStrLn "\n=================================================================================================="
