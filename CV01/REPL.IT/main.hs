module Main where
import Test.QuickCheck
-- import Trpaslici(moznosti)
-- v zalozke Shell urob:
-- cabal update
-- cabal install QuickCheck

f1 :: Int -> Integer
f1 n = toInteger n

f2 :: Integer -> Int
f2 n =  fromIntegral n :: Int


fib :: Int -> Int
fib n = if n > 1 then 
           fib (n-1) + fib (n-2)
        else 
           1

main = do print "helllllloooo!!!"
--main = do print $ length (Trpaslici.moznosti 36)
-------------- CV01
fact :: Integer -> Integer
fact n = if n == 0 then 1 else n * fact (n-1)

fact' :: Integer -> Integer
fact' 0 = 1
fact' x = x * fact' (x-1)

-- https://www.willamette.edu/~fruehr/haskell/evolution.html

fact'' :: Integer -> Integer
fact'' n = cyklus n 1
           where cyklus 0 f = f
                 cyklus i f = cyklus (i-1) (f*i)

fact''' n = foldr (*) 1 [1..n]

fact'''' n = product [1..n]

pocetCifier :: Integer -> Integer
pocetCifier n | n < 10    = 1
              | otherwise =  1+pocetCifier (n `div` 10)

pocetNul :: Integer -> Integer
pocetNul n | n == 0    = 1
           | n < 10    = 0
           | otherwise =  pocetNul (n `div` 10) + if n `mod` 10 == 0 then 1 else 0

pocetKoncovychNul :: Integer -> Integer
pocetKoncovychNul n | n == 0    = 1
                    | n < 10    = 0
                    | otherwise =  if n `mod` 10 == 0 then 1 + pocetKoncovychNul (n `div` 10)
                                   else 0
                                   
zoznamCifier :: Integer -> [Integer]
zoznamCifier 0 = []
zoznamCifier n = (n `mod` 10) : (zoznamCifier (n `div` 10))

pocetNul' :: Integer -> Integer
pocetNul' n = fromIntegral $ length $ filter (==0) $ zoznamCifier n

pocetKoncovychNul' :: Integer -> Integer
pocetKoncovychNul' n = fromIntegral $ length $ takeWhile (==0) $ zoznamCifier n
                                   
{-
*Faktorial> pocetKoncovychNul' $ fact 10
2
*Faktorial> pocetKoncovychNul' $ fact 100
24
*Faktorial> pocetKoncovychNul' $ fact 1000
249
*Faktorial> pocetKoncovychNul' $ fact 10000
2499
*Faktorial> pocetKoncovychNul' $ fact 100000
24999
-}


pocetCifier' :: Integer -> Integer
pocetCifier' n =  ceiling $ (logBase 10 (fromIntegral n))

{-
*Faktorial> pocetCifier'' (fact 100)
158
*Faktorial> pocetCifier'' (fact 1000)
179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216
*Faktorial> pocetCifier'' 1000
3
*Faktorial>
-}

hypoteza1 = quickCheck (\n -> n > 10 ==> pocetCifier n == pocetCifier' n)
hypoteza2 = quickCheckWith stdArgs {maxSuccess = 50000}  (\n -> n > 10 ==> pocetCifier n == pocetCifier' n)

-- log (a*b) = log a + log b

pocetCifierFact :: Int -> Int
pocetCifierFact n = ceiling $ sum $ map (logBase 10) [1..fromIntegral n]

log5 :: Int -> Int
log5 n = length $ takeWhile (\x -> x `mod` 5 == 0) $ iterate (`div` 5) n

koncoveNulyFact :: Int -> Int
koncoveNulyFact n = sum $ map log5 [1..n]


pocetCifierFact' :: Int -> Int
pocetCifierFact' n = ceiling $ sum $ map (logBase 2) [1..fromIntegral n]

log2 :: Int -> Int
log2 n = length $ takeWhile even $ iterate (`div` 2) n

koncoveNulyFact' :: Int -> Int
koncoveNulyFact' n = sum $ map log2 [1..n]

-- 10! 1101110101111100000000
-- pocetCifier 10 = 22
-- koncoveNuly 10 = 8

pocetCifierFact'' :: Int -> Int
pocetCifierFact'' n = length $ show $ product [1..fromIntegral n]

koncoveNulyFact'' :: Int -> Int
koncoveNulyFact'' n = length $ 
                takeWhile ((==0).(`mod` 10)) $ 
                --takeWhile (\x -> x `mod` 10 == 0) $ 
                    iterate (`div` 10) $ product [1..fromIntegral n]

