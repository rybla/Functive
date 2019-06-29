module Debug
( debug, inform
) where

import           Control.Monad

makeLogger :: Bool -> String -> (String -> IO ())
makeLogger toggle header msg = when toggle $ putStrLn $ "["++header++"] "++msg

debug = makeLogger True "$"
inform = makeLogger True ">"
