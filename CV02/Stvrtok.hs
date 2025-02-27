module Stvrtok where

import Data.List    -- prakticke funkcie nad zoznamami

import Test.QuickCheck   -- toto vam asi nepojde, kym si neodinstalujete balik QuickCheck   
                         -- cabal update
                         -- cabal install QuickCheck
                         
-- poznamka type String = [Char]
-- slova dlzky 3 nad abecedou - urobte pomocou list-comprehension

-- slova3 :: String -> [String]
slova3 :: [Char] -> [String]
slova3 abc = [[x,y,z] | x <- abc, y <- abc, z <- abc]

-- pocet slov je |abeceda|^3
-- overte hypotezu pomocou quickCheck
qchSlova3 = quickCheck(\abc -> length abc <= 20 ==> length(slova3 abc) == (length abc)^3)

------------------------------------------------------------------------------------------

-- slova dlzky k nad abecedou
-- slova :: [Char] -> Int -> [String]
slova :: String -> Int -> [String]
slova abeceda k | k == 0     = [[]]
                | otherwise  = concat [map (x:) (slova abeceda (k-1)) | x <- abeceda]

-- zistite, kolko ich je, a overte hypotezu, ze mate ich spravy pocet
pocetSlova :: [Char] -> Int -> Int 
pocetSlova abeceda k = undefined

-- overete hypotezu, ze v zozname mate spravny pocet slov
qchSlova = undefined

-- overete hypotezu, ze v zozname nemate ziadne dve rovnake slova
qchSlovaRozne = undefined

-- overete hypotezu, ze vsetky slova su dlzky k
qchSlovaDlheK = undefined

------------------------------------------------------------------------------------------

-- slova dlzky najviac k
-- slova dlzky najviac k nad abecedou
slovaNajviac :: [Char] -> Int -> [String]
slovaNajviac abeceda 0 = [""]
slovaNajviac abeceda k = slovaNajviac abeceda (k-1) ++ 
                         [ ch:w | w <-slovaNajviac abeceda (k-1), ch <- abeceda]
                         
slovaNajviac2 :: [Char] -> Int -> [String]
slovaNajviac2 abeceda k = [s | i <- [0..k], s <- slova abeceda i]

-- najdite vzorec na pocet slov dlzky najviac k nad danou abecedou
pocetSlovaNajviac abeceda k = undefined

-- overte
qchSlovaNajviac = undefined

-- opravte riesenie slovaNajviac tak, aby ste dostali spravne riesenie, aj ked neefektivne
slovaNajviacNeefektivne :: [Char] -> Int -> [String]
slovaNajviacNeefektivne abeceda k = undefined

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
slovaBezAA 0 = [""]
slovaBezAA 1 = ["a","b"]
slovaBezAA k =  ['b': s |s <- slovaBezAA (k-1)] ++ ['a':'b':s | s <- slovaBezAA (k-2)]

-- pomocou filter
slovaBezAAFilter n = filter (not.isInfixOf "aa") (slova "ab" n)

-- kolko ich je
pocetSlovaBezAA k = undefined

-- napiste quickCheck, ktory overi hypotezu
qchSlovaBezAA = quickCheck(\k1 -> let k = k1 `mod` 20 in (length (slovaBezAA k)) == (pocetSlovaBezAA k))
















                
fibonacci 0 = 1
fibonacci 1 = 2
fibonacci k = fibonacci (k-1) + fibonacci (k-2)


-- slova nad k-pismenkovou abecedou "abcd...", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA' :: String -> Int -> [String]
slovaBezAA' abeceda 0 = [""]
slovaBezAA' abeceda 1 = [[x]| x<-abeceda]
slovaBezAA' abeceda k = [x: s | x<-abeceda, x /= 'a',s<-slovaBezAA' abeceda (k-1)] 
                        ++ ['a':x:s |x<- abeceda \\['a'], s<- slovaBezAA' abeceda (k-2)]

-- najdite formulu, rek.vzorec, ktory povie, kolko je takych slov... (modifikacia Fibonacciho)

pocetSlovaBezAA' abeceda k = undefined
