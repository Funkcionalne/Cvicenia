module Stvrtok where
-- import Test.QuickCheck

pocetMoznosti n = length [ (a, b, c) | a <- delitele n, b <- delitele (n `div` a), a <= b, let c = n `div` (a * b), b <= c]

delitele n = [x | x <- [1..n `div` 2], n `mod` x == 0]

-- Int
-- Integer
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

pocetCifier :: Integer -> Integer
pocetCifier n | n == 0 = 0
              | otherwise = 1 + pocetCifier(n `div` 10)
