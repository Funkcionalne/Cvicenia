module Main where
import Test.QuickCheck


import Text.Show.Functions
import Data.List(sort)
import Test.QuickCheck.Arbitrary


-- :set +s 
{-
Foldl vs. foldr na nekonecnych / dlhych zoznamoch

1) Najdi cislo x v zozname 1..10^7 pomocou foldl a foldr
2) Striktne foldl a foldr na dlhych zoznamoch
-}

-- Ci sa X nachadza v zozname 1..10^7

najdiX :: Int -> [Int] -> Bool
najdiX x = foldl (\acc z -> z==x || acc) False

-- [x0, x1, ... xn-1] -> ([x1,x4,x7,...], [x2,x5,x8,...])
-- foldl
kazdyTretiDoKosa    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa    xs    = let (a1,a2, _) = foldl helper ([],[],0) xs
                            in (a1, a2)
    where 
      helper (acc1,acc2,i) z | i `mod` 3 == 1 = (z:acc1, acc2, i+1)
                             | i `mod` 3 == 2 = (acc1,z:acc2,i+1)
                             | otherwise = (acc1,acc2,i+1) 


-- kazdyTretiDoKosa [2..20] = ([18,15,12,9,6,3],[19,16,13,10,7,4])
-- foldr
kazdyTretiDoKosa'    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa'    xs    = let (a1, a2, _) = foldr helper ([], [], length xs - 1) xs
                             in (a1, a2)
    where
      helper z (acc1, acc2, i) | i `mod` 3 == 1 = (z:acc1, acc2, i - 1)
                               | i `mod` 3 == 2 = (acc1, z:acc2, i - 1)
                               | otherwise = (acc1, acc2, i - 1)

-- bohuzial, foldr pocita od konca...
-- kazdyTretiDoKosa' [2..20] = ([4,7,10,13,16,19],[3,6,9,12,15,18])

-- kombinuj i j a b 
-- kombinuj dva typy znakov (a,b), pricom prvy znak sa ma vyskytovat i-krat a druhy j-krat
-- kombinuj 'a' 'b' 2 2 = ["aabb","abab","abba","baab","baba","bbaa"]
kombinuj :: Char -> Char -> Int -> Int -> [String]
kombinuj a b 0 j = [replicate j b]
kombinuj a b i 0 = [replicate i a]
kombinuj a b i j = [a:xs | xs <- kombinuj a b (i-1) j] ++ [b:xs | xs <- kombinuj a b i (j-1)]


product' :: [Int] -> Int
product' = foldr (\x acc -> x * acc) 1

product'' ::  [Int] -> Int
product'' = foldl (*) 1

qch11 = quickCheck (\xs -> product xs == product' xs)

-- Sucin vsetkych okrev prvku na indexe i
-- list comprehension
suciny :: [Int] -> [Int]
suciny xs = [sucin `div` x | x <- xs]
  where
    sucin = product xs

{-
suciny [1..6] = [720,360,240,180,144,120]
[1..6] = [1,2,3,4,5,6]
-}

-- foldl
suciny' :: [Int] -> [Int]
suciny' xs = reverse $ foldl (\acc z -> (sucin `div` z):acc) [] xs
  where
      sucin = product xs

suciny'' :: [Int] -> [Int]
suciny'' xs =  
  let sucin = product xs  
  in map (sucin `div`) xs


qch1 = quickCheck (\xs -> suciny xs == suciny' xs)


-- sufixy [1..5] = [120,60,20,5,1]
-- sufixy [1..5] = [1*2*3*4*5, 2*3*4*5, 3*4*5, 4*5, 5]
sufixy :: [Int] -> [Int]
sufixy = scanr (*) 1

-- scanr
sufixy' [] = []
sufixy' xs = undefined

{-
[1  , 1, 2, 6,24, -----120]
[120,60,20, 5, 1]
[120,60,40,30,24]

-}

-- vyuzit sufixy
suciny''' :: [Int] -> [Int]
suciny''' xs = zipWith (*) suc1 suc2
  where
    suc1 = init $ reverse $ sufixy (reverse xs)
    suc2 = tail $ sufixy xs 

-- qch4 = quickCheck(\xs -> suciny''' xs == suciny' xs)

-- implementacia foldl cez foldr
myfoldl f z xs = undefined
-- implementacia cez foldl
myfoldr f z xs = undefined


main :: IO ()
main = do
  print "MAIN"
