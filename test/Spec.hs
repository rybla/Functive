import           Control.Monad.Trans.State
import           Grammar
import           Typing

e = ExprName $ name "e"
f = ExprName $ name "f"

t = TypeName $ name "t"
n = name "n"
m = name "m"

prgm = Prgm [ Definition n t (ExprAppl e f)
            , Definition m t e ]

main :: IO ()
main = do
  putStrLn "\n==================================================================================================\n"

  runStateT (checkPrgm prgm) emptyTypeContext

  putStrLn "\n=================================================================================================="
