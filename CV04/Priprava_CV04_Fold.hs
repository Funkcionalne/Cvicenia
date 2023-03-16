module Priprava_CV04_Fold where
import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)
import Test.QuickCheck.Arbitrary

-- [x0, x1, ... xn-1] -> ([x1,x4,x7,...], [x2,x5,x8,...])
kazdyTretiDoKosa    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa    xs    = (reverse vs, reverse us)
                          where
                          (_, vs, us) =    foldl ( \(i, as, bs) -> \x -> if i `mod` 3 == 0 then (i+1, as, bs)
                                                                         else if i `mod` 3 == 1 then (i+1, x:as, bs)
                                                                         else (i+1, as, x:bs)
                                ) (0, [], []) xs

-- kazdyTretiDoKosa [2..20] = ([18,15,12,9,6,3],[19,16,13,10,7,4])

kazdyTretiDoKosa'    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa'    xs    = (vs, us)
                          where
                          (_, vs, us) =    foldr ( \x -> \(i, as, bs) -> if i `mod` 3 == 0 then (i+1, as, bs)
                                                                         else if i `mod` 3 == 1 then (i+1, x:as, bs)
                                                                         else (i+1, as, x:bs)
                                              ) (0, [], []) xs
-- bohuzial, foldr pocita od konca...
-- kazdyTretiDoKosa' [2..20] = ([4,7,10,13,16,19],[3,6,9,12,15,18])

kazdyTretiDoKosa''    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa''    xs    = (vs, us)
                          where
                          (_, vs, us) =    foldr ( \x -> \(i, as, bs) -> if i `mod` 3 == 0 then (i+1, as, bs)
                                                                         else if i `mod` 3 == 1 then (i+1, x:as, bs)
                                                                         else (i+1, as, x:bs)
                                              ) (0, [], []) (reverse xs)

-- kazdyTretiDoKosa'' [2..20] = ([18,15,12,9,6,3],[19,16,13,10,7,4])
                                                                
kazdyTretiDoKosa'''    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa'''    xs    = foldl (\(as, bs) -> \(i,x) -> if  i `mod` 3 == 0 then (as, bs)
                                                             else if i `mod` 3 == 1 then (x:as, bs)
                                                             else (as, x:bs)
                                ) ([],[]) poleAkoVPythone
                        where poleAkoVPythone = zip [0..] (reverse xs)
                                
-- kazdyTretiDoKosa''' [2..20] = ([4,7,10,13,16,19],[3,6,9,12,15,18])


suciny :: [Int] -> [Int]
suciny xs = [ product xs `div` x | x <- xs] 
{-
suciny [1..6] = [720,360,240,180,144,120]
[1..6] = [1,2,3,4,5,6]
-}

--- nesmiete delit
suciny' :: [Int] -> [Int]
suciny' xs = [ product [ xs!!j | j <- [0..length xs-1], j /= i] | i <- [0..length xs-1] ] 

qch1 = quickCheck(\xs -> suciny xs == suciny' xs)
{-
*Priprava_CV03_fold> qch1
*** Failed! Exception: 'divide by zero' (after 6 tests and 2 shrinks):
[0]
-}


suciny'' :: [Int] -> [Int]
suciny'' xs = zipWith (*) (sufixy xs) (reverse (sufixy (reverse xs)))

sufixy :: [Int] -> [Int]
sufixy [] = []
sufixy [_] = [1]
sufixy (x:xs) = let pom = sufixy xs in (head xs * head pom) : pom

{-
 suciny'' [1..6]
[720,360,240,180,144,120]
-}

qch2 = quickCheck(\xs -> suciny'' xs == suciny' xs)


sufixy' :: [Int] -> [Int]
sufixy' [] = []
sufixy' xs = tail $ scanr (*) 1 xs

qch3 = quickCheck(\xs -> sufixy xs == sufixy' xs)

suciny''' :: [Int] -> [Int]
suciny''' xs = zipWith (*) (sufixy' xs) (reverse (sufixy' (reverse xs)))

qch4 = quickCheck(\xs -> suciny''' xs == suciny' xs)


myfoldl f z xs = foldr (\x -> \rec -> f rec x) z (reverse xs)
myfoldl' f z xs = foldr (flip f) z (reverse xs)

qch5 = quickCheck( (\xs -> \f -> \z -> (myfoldl f z xs) == (foldl f z xs))::[Int]->(Int->Int->Int)->Int-> Bool )
qch5' = quickCheck( (\xs -> \f -> \z -> (myfoldl f z xs) == (foldl f z xs))::[Int]->(Int->Int->Int)->Int-> Bool )


myfoldr f z xs = foldl (\acc -> \x -> f x acc) z (reverse xs)
myfoldr' f z xs = foldl (flip f) z (reverse xs)

qch6 = quickCheck( (\xs -> \f -> \z -> (myfoldr f z xs) == (foldr f z xs))::[Int]->(Int->Int->Int)->Int-> Bool )
qch6' = quickCheck( (\xs -> \f -> \z -> (myfoldr f z xs) == (foldr f z xs))::[Int]->(Int->Int->Int)->Int-> Bool )
