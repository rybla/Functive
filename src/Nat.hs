module Nat
( Nat(O, S)
) where

data Nat = O | S Nat
  deriving (Eq)

instance Show Nat where
  show O     = "O"
  show (S n) = "S"++show n

instance Enum Nat where
  -- toEnum
  toEnum 0 = O -- negative values are undefined
  toEnum i = S (toEnum $ i-1)
  -- fromEnum
  fromEnum O     = 0
  fromEnum (S n) = 1 + fromEnum n

instance Num Nat where
  -- (+)
  O   + n = n
  S n + m = S (n + m)
  -- (-)
  O   - n = O
  n   - O = n
  S n - S m = n - m
  -- (*)
  O     * n = O
  (S n) * m = m + n * m
  -- abs
  abs = id
  -- signum
  signum O     = O
  signum (S n) = S O
  -- fromInteger
  fromInteger 0 = O -- negative values are undefined
  fromInteger i = S (fromInteger $ i-1)
