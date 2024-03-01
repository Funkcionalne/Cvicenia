module Stvrtok where

import Data.List    -- prakticke funkcie nad zoznamami

import Test.QuickCheck   -- toto vam asi nepojde, kym si neodinstalujete balik QuickCheck   
                         -- cabal update
                         -- cabal install QuickCheck
                         
-- slova dlzky 3 nad abecedou - urobte pomocou listcomprehension
slova3 :: [Char] -> [String]
slova3 abc = [[x,y,z]|x<-abc, y<-abc, z<-abc]

-- pocet slov je |abeceda|^3
-- overte hypotezu pomocou quickCheck
qchSlova3 = quickCheck(\abeceda -> (length abeceda < 100) ==> (length abeceda) ^ 3 == (length $ slova3 abeceda))

------------------------------------------------------------------------------------------

-- slova dlzky k nad abecedou
-- slova :: [Char] -> Int -> [String]
slova :: String -> Int -> [String]
slova abeceda 0 = [""]
slova abeceda k = [x:y| x <- abeceda, y <- slova abeceda (k-1)]

-- zistite, kolko ich je, a overte hypotezu, ze mate ich spravy pocet
pocetSlova :: [Char] -> Int -> Int 
pocetSlova abeceda k = length abeceda ^ k

qchSlova = quickCheck(\abeceda -> \k1 -> let k = k1 `mod` 8 in (length abeceda < 10) ==> (pocetSlova abeceda k) == (length $ slova abeceda k))

-- overete hypotezu, ze v zozname nemate ziadne dve rovnake slova

qchSlovaRozne = quickCheck(\abeceda -> \k1 -> let k = k1 `mod` 8 in (length abeceda < 10) ==> 
                let  x = slova abeceda k in length (nub (x)) == length x)

-- overete hypotezu, ze vsetky slova su dlzky k

qchSlovaDlheK = quickCheck(\abeceda -> \k1 -> let k = k1 `mod` 8 in (length abeceda < 10) ==> 
                let  xs = slova abeceda k in null $ filter (\x -> (length x) /= k) xs)

------------------------------------------------------------------------------------------

-- slova dlzky najviac k
-- slova dlzky najviac k nad abecedou
slovaNajviac :: [Char] -> Int -> [String]
slovaNajviac abeceda 0 = [""]
slovaNajviac abeceda k = "":[x:y|x<-abeceda,y<-slovaNajviac abeceda (k-1)]

-- najdite vzorec na pocet slov dlzky najviac k nad danou abecedou
pocetSlovaNajviac abeceda k = (((length abeceda) ^ (k+1)) - 1) `div` ((length abeceda) -1)

-- opravte riesenie slovaNajviac tak, aby ste dostali spravne riesenie, aj ked neefektivne
slovaNajviacNeefektivne :: [Char] -> Int -> [String]
slovaNajviacNeefektivne abeceda k = concat [slova abeceda i|i<-[0..k]]

-- overte hypotezu, ze ich je uz spravny pocet
qchSlovaNajviacNeefektivne = undefined

-- a teraz slovaNajviac napiste slusne a efektivnejsie :)
slovaNajviacEfektivne :: [Char] -> Int -> [String]
slovaNajviacEfektivne abeceda k = undefined

-- overte hypotezu, ze ich je uz spravny pocet
qchSlovaNajviacEfektivne = undefined

-----------------------------------------------------------------------------------

-- slova nad abecedou "ab", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA :: Int -> [String]
slovaBezAA 0 = [[]]
slovaBezAA 1 = ["a","b"]
slovaBezAA k =  ['b':y|y<-slovaBezAA (k-1)]++
                [x:'b':y|x<-"a", y<-slovaBezAA (k-2)]

-- pomocou filter
slovaBezAAFilter n = filter (not.isInfixOf "aa") (slova "ab" n)

-- kolko ich je
pocetSlovaBezAA k = fibonacci k

-- napiste quickCheck, ktory overi hypotezu
qchSlovaBezAA = quickCheck(\k1 -> let k = k1 `mod` 20 in (length (slovaBezAA k)) == (pocetSlovaBezAA k))






                
fibonacci 0 = 1
fibonacci 1 = 2
fibonacci k = fibonacci (k-1) + fibonacci (k-2)


-- slova nad k-pismenkovou abecedou "abcd...", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA' :: String -> Int -> [String]
slovaBezAA' abeceda k = undefined

-- najdite formulu, rek.vzorec, ktory povie, kolko je takych slov... (modifikacia Fibonacciho)

pocetSlovaBezAA' abeceda k = undefined
