import           Control.Monad.Trans.State
import           Grammar
import           Nat
import           Typing

x = ExprName . Name $ "x"
y = ExprName . Name $ "y"
f = ExprName . Name $ "f"

t = TypeName . Name $ "t"
n = Name "n"
m = Name "m"

prgm = Prgm [ Definition n t
              $ ExprAppl (ExprFunc (Name "x") (ExprAppl f x)) x
            ]

header = "\n\n==================================================================================================\n\n"

main :: IO ()
main = do
  putStr header
  runStateT (checkPrgm prgm) emptyTypeContext
  putStr header
