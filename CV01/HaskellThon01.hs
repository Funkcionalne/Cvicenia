import Data.List

myLastButOne :: [Int] -> Int
-- myLastButOne xs = head (tail (reverse xs))
-- myLastButOne xs = last (init xs)
-- myLastButOne' = last . init
-- myLastButOne xs = reverse xs !! 1
myLastButOne [x, _] = x 
myLastButOne (_:xs) = myLastButOne xs

myReverse :: [t] -> [t]
myReverse [] = []
myReverse (x:xs) =   (myReverse xs) ++ [x]

isPalindrome :: (Eq t) => [t] -> Bool
--isPalindrome [] = True
--isPalindrome [x] = True
--isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome (init xs) ) 

-- isPalindrome xs | length xs < 2 = True
--                | otherwise = ((head xs) == (last xs)) && (isPalindrome (tail  (init xs)) ) 

isPalindrome xs = xs == (reverse xs)



nub' :: [Int] -> [Int]
nub' [] = []
nub' (x:xs) = (if elem x xs then [] else [x]) ++ nub' xs
-- nub' xs = map head (group (sort xs))

duplikuj :: [Int] -> [Int]
--duplikuj [] = []
--duplikuj (x:xs) = x:x: (duplikuj xs)
--duplikuj xs = xs ++ xs -- nedokoncene riesenie 
duplikuj xs = concat [[x,x] | x <- xs]

sudelitelne :: Int -> Int -> Bool
sudelitelne x y = 1 /= length (intersect (delitele x) (delitele y))

delitele :: Int -> [Int]
delitele n = [d | d <- [1..n], (mod n d) == 0]

sudelitelne' :: Int -> Int -> Bool
sudelitelne' x y = 1 /= length (intersect (delitele' x) (delitele' y))
                where 
                delitele' n = [d | d <- [1..n], (mod n d) == 0]


jePrefix :: [Int] -> [Int] -> Bool     
jePrefix xs ys =  (take (length xs) ys)  == xs   

median :: [Int] -> Int
median [] = 0
median xs = (sort xs) !! ((length xs) `div` 2)
-- median xs = (sort xs) !! (div (length xs) 2)

prienik :: [Int] -> [Int] -> [Int]
prienik xs ys = nub [x | x <-xs, elem x ys]

treti :: [Int] -> Int -- treti najvacsi
treti xs | length xs < 3 = 0
         | otherwise = (reverse (sort xs)) !! 2
