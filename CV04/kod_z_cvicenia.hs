module Main where

import Data.List

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldl (\acc x ->  acc ++ [f x]) [] xs 



filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x:acc else acc) [] xs



myFoldr' :: (a -> b -> b) -> b -> [a] -> b
myFoldr' p acc xs = foldr (p) acc xs



-- sumy [1, 2, 3, 4, 5] == [1, 3, 6, 10, 15]
sumy :: [Int] -> [Int]
sumy [] = []
sumy xs = tail $ foldl (\acc x -> acc ++ [last acc + x]) [0] xs 



kazdeTretie :: [a] -> [a]
kazdeTretie xs = fst $ foldl (\(acc,c) x -> if c == 2 then (acc++[x],0) else (acc,c+1)) ([],0) xs 



-- kompoziciaFunkcii [(+1), (*2)] 10 == 21
kompoziciaFunkcii :: [Int -> Int] -> Int -> Int
kompoziciaFunkcii = undefined

nafukni :: [a] -> [a]
nafukni xs = undefined

-- nafukni ['a', 'b', 'c'] -- ['a', 'b', 'b', 'c', 'c', 'c']
-- nafukni [] -- []

nafukniR :: [a] -> [a]
nafukniR = undefined

-- nafukni ['a', 'b', 'c'] -- ['a', 'a', 'a', 'b', 'b', 'c']
-- nafukni [] -- []

-- Huffmanove stromy
type Weight = Int

data HTree = Leaf (Weight, Char)
           | Node HTree Weight HTree deriving (Eq, Show)

-- frekvencna tabulka
ex1 :: [(Weight, Char)]
ex1 = [(8, 'G'),(9, 'R'),(11, 'A'),(13,'T'),(17,'E')]

ex1Tree :: [HTree]
ex1Tree = map Leaf ex1

weight :: HTree -> Int
weight (Leaf (w, _)) = w
weight (Node _ w _) = w

instance Ord HTree where
  a <= b = weight a <= weight b

combine :: [HTree] -> [HTree]
combine [] = []
combine [a] = [a]
combine (x:y:xs) = combine . sort $ Node x (weight x + weight y) y : xs

build :: [HTree] -> HTree
build = head . combine

main :: IO ()
main = putStrLn "hallo"
