import           Control.Monad.Trans.State
import           Grammar
import           Nat
import           Typing


{-

  # Stock

-}

[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] =
  map Name
    ["a","b","c","d","e","f","g","h","i","j","k","l","m"
    ,"n","o","p","q","r","s","t","u","v","w","x","y","z"]

prgm = Prgm [ Definition n (TypeName t)
              $ ExprAppl
                  (ExprFunc n
                    (ExprAppl
                      (ExprName f)
                      (ExprName x)))
                  (ExprName x)
            , Definition m (TypeFunc (TypeName t) (TypeName t))
              $ ExprFunc x (ExprName x)
            ]

{-

  # Main

-}

header = "\n\n"++replicate 98 '='++"\n\n"

main :: IO ()
main = do
  putStr header
  runStateT (checkPrgm prgm) emptyTypeContext
  putStr header
