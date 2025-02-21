module Faktorial where

{-
.....
-}
-- Int
-- Integer
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)
