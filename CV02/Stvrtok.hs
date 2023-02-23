module Stvrtok where

import Data.List    -- prakticke funkcie nad zoznamami

import Test.QuickCheck   -- toto vam asi nepojde, kym si neodinstalujete balik QuickCheck   
                         -- cabal update
                         -- cabal install QuickCheck
                         
-- slova dlzky 3 nad abecedou - urobte pomocou listcomprehension
slova3 :: [Char] -> [String]
slova3 abeceda = [[ch1, ch2, ch3] | ch1 <- abeceda, ch2 <- abeceda, ch3 <- abeceda]

-- pocet slov je |abeceda|^3
-- overte hypotezu pomocou quickCheck
qchSlova3 = quickCheck (\abeceda -> length abeceda < 20 ==> (length abeceda)^3 == length (slova3 abeceda) )

------------------------------------------------------------------------------------------

-- slova dlzky k nad abecedou
-- slova :: [Char] -> Int -> [String]
slova :: String -> Int -> [String]
slova abeceda 0 = [""]
slova abeceda k = [y:x | x <- slova abeceda (k-1), y <- abeceda]

-- zistite, kolko ich je, a overte hypotezu, ze mate ich spravy pocet
pocetSlova :: [Char] -> Int -> Int 
pocetSlova abeceda k = (length abeceda)^k

qchSlova = quickCheck (\abeceda -> \k -> length abeceda < 20 && k <= 6 && k >= 0 ==> (length abeceda)^k == length (slova abeceda k) )

-- overete hypotezu, ze v zozname nemate ziadne dve rovnake slova

qchSlovaRozne = quickCheck (\abeceda -> \k -> length abeceda < 10 && k <= 4 && k >= 0 && length abeceda == length (nub(abeceda))==> let l = slova abeceda k in length l == length (nub l) )

-- overete hypotezu, ze vsetky slova su dlzky k

qchSlovaDlheK = quickCheck (\abeceda -> \k -> length abeceda < 10 && k <= 4 && k >= 0 && length abeceda == length (nub(abeceda))==> all (\s -> length s == k) (slova abeceda k))

------------------------------------------------------------------------------------------

-- slova dlzky najviac k
-- slova dlzky najviac k nad abecedou, zle riesenie (zistite, preco je to zle riesenie)
slovaNajviac :: [Char] -> Int -> [String]
slovaNajviac abeceda 0  = [[]]
slovaNajviac abeceda k = let p = slovaNajviac abeceda (k-1) in 
                         p ++ [ ch:w | ch <- abeceda, w <-p]

-- najdite vzorec na pocet slov dlzky najviac k nad danou abecedou
pocetSlovaNajviac abeceda k = ()

-- opravte riesenie slovaNajviac tak, aby ste dostali spravne riesenie, aj ked neefektivne
slovaNajviacNeefektivne :: [Char] -> Int -> [String]
slovaNajviacNeefektivne abeceda k = nub $ slovaNajviacNeefektivne abeceda k

-- overte hypotezu, ze ich je uz spravny pocet
qchSlovaNajviacNeefektivne = undefined

-- a teraz slovaNajviac napiste slusne a efektivnejsie :)
slovaNajviacEfektivne :: [Char] -> Int -> [String]
slovaNajviacEfektivne abeceda k = concat [ slova abeceda d | d <- [0..k]]

-- overte hypotezu, ze ich je uz spravny pocet
qchSlovaNajviacEfektivne = quickCheck (\abeceda -> \k -> length abeceda < 10 && k <= 4 && k >= 0 && length abeceda == length (nub(abeceda)) && length abeceda > 1 ==> let q = length abeceda in (q^(k+1) - 1) `div` (q - 1) == length ( slovaNajviacEfektivne abeceda k) )


-----------------------------------------------------------------------------------

-- slova nad abecedou "ab", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA :: Int -> [String]
slovaBezAA 0 = [ [] ]
slovaBezAA 1 = [ "a", "b" ]
slovaBezAA k = ['b':w | w <- slovaBezAA (k-1)] ++
               [ "ab" ++ w | w <- slovaBezAA (k-2) ]

slovaBezAAFilter n = filter (not.isInfixOf "aa") $ slova "ab" n

-- kolko ich je
pocetSlovaBezAA k = undefined

-- napiste quickCheck, ktory overi hypotezu
qchSlovaBezAA = undefined






                
fibonacci 0 = 1
fibonacci 1 = 2
fibonacci k = fibonacci (k-1) + fibonacci (k-2)


-- slova nad k-pismenkovou abecedou "abcd...", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA' :: String -> Int -> [String]
slovaBezAA' abeceda k = undefined

-- najdite formulu, rek.vzorec, ktory povie, kolko je takych slov... (modifikacia Fibonacciho)

pocetSlovaBezAA' abeceda k = undefined
