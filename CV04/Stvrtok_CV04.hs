module Main where

import Data.List

-- definujte map pomocou foldr/l
map' :: (a -> b) -> [a] -> [b]
map' f xs = undefined


-- definujte filter pomocou foldr/r
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = undefined





-- sumy [1, 2, 3, 4, 5] == [1, 3, 6, 10, 15]
sumy :: [Int] -> [Int]
sumy [] = []
sumy xs = tail $ foldl (\acc x -> acc ++ [last acc + x]) [0] xs 


-- vrati kazdy treti prvok povodneho zoznamu
kazdeTretie :: [a] -> [a]
kazdeTretie xs = undefined


-- kompoziciaFunkcii [(+1), (*2)] 10 == 21
kompoziciaFunkcii :: [Int -> Int] -> Int -> Int
kompoziciaFunkcii = undefined

-- toto je na rozcvicku :)
nafukni :: [a] -> [a]
nafukni xs = undefined

-- nafukni ['a', 'b', 'c'] -- ['a', 'b', 'b', 'c', 'c', 'c']
-- nafukni [] -- []

-- alebo to je na rozcvicku :)
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
combine = undefined

build :: [HTree] -> HTree
build = head . combine

main :: IO ()
main = putStrLn "hello"
