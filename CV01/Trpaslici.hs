module Trpaslici where

moznosti :: Int -> [(Int,Int,Int)]
moznosti n =  [(x,y,n `div` (x*y)) | x <- [1..n], y <- [x..n], 
                n `mod` (x*y) == 0, y <= n `div` (x*y)]

pocetMoznosti :: Int -> Int
pocetMoznosti n = length (moznosti n)

delitele    :: Int -> [Int]
delitele n = [ i | i <- [1..n], n `mod` i == 0]

-- toto na cviku nebolo, treba to trochu vytrapit...
sqrti n = floor (sqrt $ fromIntegral n)

delitele' :: Int -> [Int]
delitele' n = [x | (x,y) <- pom] ++ [y | (x,y) <- pom]
                where pom = [(i, n `div` i) | i <- [1..(sqrti n)],
                             n `mod` i == 0]

moznosti' :: Int -> [(Int,Int,Int)]
moznosti' n =  [(x,y,z) | x <- pom, y <- pom, x <= y, z <- pom, 
                          y <= z, x*y*z == n]
                where pom = delitele n

pocetMoznosti' :: Int -> Int
pocetMoznosti' n = length (moznosti' n)

{-
*Trpaslici> length (moznosti 36)
8
*Trpaslici> length (moznosti 1024)
-}

