import           Control.Monad.Trans.State
import           Grammar
import           Typing

x = ExprName $ name "x"
y = ExprName $ name "y"
f = ExprName $ name "f"

t = TypeName $ name "t"
n = name "n"
m = name "m"

prgm = Prgm [ Definition n t
              $ ExprAppl (ExprFunc (name "x") (ExprAppl f x)) x
            ]

main :: IO ()
main = do
  putStrLn "\n==================================================================================================\n"

  runStateT (checkPrgm prgm) emptyTypeContext

  putStrLn "\n=================================================================================================="
