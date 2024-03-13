module Main where

import Data.List.Split (splitOn)
import qualified Data.Array as A
import qualified Data.Map as M

-- :set -XTypeApplications
-- :t (+) @Int


-- Using fold r double each element in the list
double :: [a] -> [a]
double = reverse . foldl (\acc x -> x:x:acc) []  -- acc ++ [x,x]

-- Using fold each element x in the list repeat x times
-- unroll [2, 1, 3] = [2,2,1,3,3,3]
unroll :: [Int] -> [Int]
unroll = foldl (\acc x -> acc ++ replicate x x) []

--recursive redinition
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Using fold(l/r)
fibFold :: Int -> Int
fibFold 0 = 1
fibFold 1 = 1
fibFold n = fst $ foldl (\(x, y) _ -> (y, x + y)) (1, 1) [1..n]

-- Using map (dynamic programing)
fibs, fibs', fibs'' :: [Integer]
fibs = 1:1: map (\i -> fibs!!(i-1) + fibs!!(i-2)) [2..]

-- Using ZipWith on infinite lists
fibs' = 1:1: zipWith (+) fibs' (tail fibs')

-- Scanl
fibs'' = map fst $ scanl (\(x, y) _ -> (y, x + y)) (1, 1) [1..]

fibs''' =  scanl1 (+) (1:1:fibs''')

fibArray :: Integer -> Integer
fibArray n = mem A.! n
    where mem = A.array (1,n) ((1,1):(2,1):[(i,mem A.! (i-1) + mem A.! (i-2))|i <- [3..n]])

{-
AOC 2023 day 12
s = (pocet spracovanych znakov, pocet spracovanych cisel, kolko som videl # za sebou)
D[z, n, c] = D [] 
D[z, n, c] = 
    | S[z] == '.' = D[z+1, n, 0]
    | S[z] == '.' && c == N[n] = D[z+1, n+1, 0]
    | otherwise = 0  

    | S[z] == "#" && c < N[n] = D[z+1, n, c+1]
    | oterwise = 0

    | S[z] == "?" -> S[z] = '#' -> D[z, n, c] + S[z] = '.' -> D[z, n, c]
                

-}

day12 :: (String, [Int]) -> Int
day12 (xs, ns) = mem A.! (0,0,0)
    where
        lx = length xs
        ln = length ns
        mem = A.array ((0,0,0), (lx, ln, maximum ns)) [((z, n, c), aux z n c ch nn) 
            | (z, ch) <- zip [0..] (xs ++ "X"), -- add stopping element 
              (n, nn) <- zip [0..] (ns ++ [0]), 
              c <- [0..maximum ns]]
        aux z n c char num 
            | char == '.' && c == num = mem A.! (z+1, n+1, 0)
            | char == '.' && c == 0 = mem A.! (z+1, n, 0)
            | char == '#' && c < num = mem A.! (z+1, n, c+1)
            | char == '?' = aux z n c '.' num + aux z n c '#' num
            | z == lx && n == ln && c == 0 = 1
            | z == lx-1 && n == ln -1 && num == c = 1
            | n == ln   && c == 0     && char /= '#' = mem A.! (z+1, n, 0)
            | otherwise = 0


parseInput :: String -> [(String, [Int])]
parseInput = map ((\[m, x] -> (m, map read (splitOn "," x))) . words) . lines

main :: IO ()
main = do
    parsed <- parseInput <$> readFile "small.in"
    print  parsed
    print $ day12 $ last parsed
