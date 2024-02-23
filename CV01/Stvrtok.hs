module Stvrtok where
import Test.QuickCheck

pocetMoznosti n = undefined

-- Int
-- Integer
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

{-
fact' :: Integer -> Integer
fact' n = fact'' 1 n 1

fact'' :: Integer -> Integer -> Integer -> Integer
fact'' i n p = if i <= n then fact'' (i+1) n (p*i) else p
-}


{-
fact' :: Integer -> Integer
fact' n = fact'' 1 n 1
          where 
                pom = n+1
                fact'' :: Integer -> Integer -> Integer -> Integer
                fact'' i n p | i <= n = fact'' (i+1) n (p*i) 
                             | True = p
-}

fact' :: Integer -> Integer
fact' n = fact'' 1 1
          where 
                fact'' :: Integer -> Integer -> Integer
                fact'' i p | i <= n    = fact'' (i+1) (p*i) 
                           |otherwise  = p



pocetCifier :: Integer -> Integer
pocetCifier n | n >= 10 = 1 + pocetCifier (n `div` 10)
              | otherwise = 1
              
--pocetCifier' :: Integer -> Integer
pocetCifier' n = ceiling (logBase 10 (n+1))

pocetKN :: Integer -> Integer
pocetKN 0 = 1
pocetKN n | (n `mod` 10) == 0 = 1+ pocetKN (n `div` 10)
          | otherwise = 0

log5 :: Integer -> Integer
log5 0 = 0
log5 n | (n `mod` 5) == 0 = 1 + log5 (n `div` 5)
       | otherwise = 0
              