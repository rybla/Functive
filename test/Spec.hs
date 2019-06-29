import           Control.Monad.Trans.State
import           Grammar
import           Nat
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

header = "\n\n==================================================================================================\n\n"

main :: IO ()
main = do
  putStr header
  -- runStateT (checkPrgm prgm) emptyTypeContext
  putStr header
