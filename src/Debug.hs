module Debug where

import           Control.Monad

makeLogger :: Bool -> String -> (String -> IO ())
makeLogger toggle hdr msg = when toggle $ putStrLn $ "["++hdr++"] "++msg

debug = makeLogger True "$"
inform = makeLogger True ">"

buffer :: Int -> String -> String
buffer buf str = str++replicate (max 0 $ buf-length str) ' '
