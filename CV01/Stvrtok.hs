module Stvrtok where
import Test.QuickCheck

pocetMoznosti n = undefined

-- Int
-- Integer
fact :: Integer -> Integer
fact n | n < 0 = error "je zaporne"
       | n <= 1 = 1
       | otherwise = n * fact (n-1)


fact' :: Integer -> Integer
fact' n = fact'' 1 n 1

fact'' :: Integer -> Integer -> Integer -> Integer
fact'' i n p = if i <= n then fact'' (i+1) n (p*i) else p


fact2 :: Integer -> Integer
fact2 n = fact1 n 1
            where 
                fact1 :: Integer -> Integer -> Integer
                fact1 i p = if i > 1 then fact1 (i-1) (p*i) else p


{-
fact' :: Integer -> Integer
fact' n = fact'' 1 n 1
          where 
                pom = n+1
                fact'' :: Integer -> Integer -> Integer -> Integer
                fact'' i n p | i <= n = fact'' (i+1) n (p*i) 
                             | True = p
-}

{-
fact' :: Integer -> Integer
fact' n = fact'' 1 1
          where 
                fact'' :: Integer -> Integer -> Integer
                fact'' i p | i <= n    = fact'' (i+1) (p*i) 
                           |otherwise  = p
-}

-- pocet dekadickych cifier cisla
pocetCifier :: Integer -> Integer
pocetCifier n 
            | n <10 = 1 
            | otherwise = 1+pocetCifier (div  n 10)
              
pocetCifier' :: Integer -> Integer
pocetCifier' n = floor(1+logBase 10 (fromInteger n))

hypoteza = quickCheck( \n -> n>0 ==> pocetCifier n == pocetCifier' n)

-- pocet koncovych nul cisla, teda cislo je v tvare ......000000, potom vysledok je 6
pocetKN :: Integer -> Integer
pocetKN n | n == 0 = 1
          | n < 10 = 0
          | otherwise =  (if n `mod` 10 == 0 then 1 else 0) + pocetKN (n `div` 10) 
                      

zoznamCifier :: Integer -> [Integer]
zoznamCifier n = undefined

pocetKNF :: Integer -> Integer
pocetKNF n = pocetKN (fact n)

log5 :: Integer -> Integer
log5 n = if n `mod` 5 == 0 then 1+log5(n `div` 5) 
                           else 0