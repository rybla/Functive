module Debug
( debug
) where

import           Control.Monad

finalDEBUG :: Bool
finalDEBUG = True

debug :: String -> IO ()
debug str = when finalDEBUG $ putStrLn $ "[$] "++str
