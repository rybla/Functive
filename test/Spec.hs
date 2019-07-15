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
              $ ExprAppl (ExprFunc (Name "x") (ExprAppl f x)) x ]

-- expr = ExprAppl (ExprFunc (Name "x") (ExprAppl f x)) x
expr = ExprAppl f x

header = "\n\n"++replicate 98 '='++"\n\n"

main :: IO ()
main = do
  putStr header
  -- runStateT (checkPrgm prgm) emptyTypeContext
  runStateT (checkExpr expr) emptyTypeContext
  putStr header
