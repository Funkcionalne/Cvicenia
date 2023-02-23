module Sortiky where

import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)

-- naprogramuje jeden prechod bubble sort algoritmu
bubble :: [Int] -> [Int]
bubble   []                      = []
bubble   [x]                     = [x]
bubble   (x:xs@(y:ys))  | x > y  = y : (bubble (x:ys))
                        | otherwise   = x : (bubble xs)
                       
qch1 = quickCheck(\xs -> (xs /= []) ==> last (bubble xs) == maximum xs)

ordered :: [Int] -> Bool
ordered []  = True
ordered [_] = True
ordered (x:xs@(y:ys))  = x <= y && ordered xs

qchX = quickCheck(\xs -> ordered(sort xs))
qchY = verboseCheck(\xs -> ordered(sort xs))

bubbleSort  :: [Int]->[Int]
bubbleSort xs = (iterate bubble xs)!!(length xs)
{--
"?: " bubbleSort [4,3,4,6,7,4,3,1,1,2,3,4,5,6,7,8,9,0,5,3,2,3,2,3,4,5,6,7,1,2,2,0,9,12,11]
[0,0,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,6,7,7,7,8,9,9,11,12]
--}
qch2 = quickCheck(\xs -> ordered(bubbleSort xs))

insert  :: Int -> [Int]->[Int]
insert x [] = [x]
insert x (y:xs) | x < y = x:y:xs    
                | otherwise = y : (insert x xs)

qch3 = quickCheck(\x -> \xs -> ordered xs ==> ordered(insert x xs))

insertSort  :: [Int]->[Int]
insertSort xs = insertSort xs []
      where 
          insertSort [] usortene     = usortene
          insertSort (x:xs) usortene = insertSort xs (insert x usortene)

qch4 = quickCheck(\xs -> ordered(insertSort xs))
     
merge :: (Ord t) => [t] -> [t] -> [t]
merge []  ys    = ys
merge xs  []    = xs
merge a@(x:xs) b@(y:ys) | x < y = x : merge xs b
                        | otherwise = y : merge a ys
                
qch5 = verboseCheck(\xs -> \ys -> 
        length xs < 6 && length ys < 6 &&
        ordered xs  && ordered ys
        ==> ordered(merge xs ys))
                