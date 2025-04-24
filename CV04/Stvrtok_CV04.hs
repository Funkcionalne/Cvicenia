module Priprava_CV04_Fold where
import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)
import Test.QuickCheck.Arbitrary

-- definujte map pomocou foldr/l
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x-> \r-> f x:r) [] xs


-- definujte filter pomocou foldr/r
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldl (\acc-> \x-> if p x then (acc ++ [x]) else acc) [] xs

---- foldy

-- definujte pomocou foldl/foldr
-- priemer zoznamu cisel, ale prejdete zoznamom len raz,
-- takze riesenie typu avg xs = sum xs / length xs nie je okay
avg :: [Double] -> Double
avg xs = let (s,c) = foldl (\(s,c) -> \x->(s+x,c+1)) (0,0) xs 
            in s/c

avg'' :: [Double] -> Double
avg'' xs = uncurry (/) $ foldl (\(s,c) -> \x->(s+x,c+1)) (0,0) xs 
            
-- priemer prvkov matice, resp. 'dvojrozmerneho' pola
avg' :: [[Double]] -> Double
avg' xss = uncurry (/) $ foldl (\(s,c)-> \xs-> plus (pom xs) (s,c)) (0,0) xss

plus (a,b) (c,d) = (a+c,b+d)
pom xs = foldr (\x -> \(s,c)-> (s+x,c+1)) (0,0) xs
-- [[1.0,3.0],[1.0]]

-- definujte pomocou foldl aj foldr
-- [x0, x1, ... xn-1] -> ([x1,x4,x7,...], [x2,x5,x8,...])
kazdyTretiDoKosa    :: [t] -> ([t], [t])
kazdyTretiDoKosa   xs = foldr (\(x,i)-> \(as',bs)-> 
                                    if mod i 3 == 0 then (as', bs)
                                    else if mod i 3 ==1 then (x:as',bs) else (as',x:bs)
                              ) ([],[]) ( zip xs [0..])
                              
kazdyTretiDoKosa'    :: [t] -> ([t], [t])
kazdyTretiDoKosa'   xs =let (a,b,c)= foldr (\x-> \(as',bs,i)-> 
                                    if mod i 3 == 0 then (as', bs, i+1)
                                    else if mod i 3 ==1 then (x:as',bs,i+1) else (as',x:bs,i+1)
                              ) ([],[],0) xs 
                                in (a,b)

-- ["a","b","c","d","e"]

foldr' f z xs = foldl (\acc-> \x -> f x acc) z $ reverse xs

foldr'' f z xs = foldl (flip f) z $ reverse' xs
            where 
                reverse' xs = foldl  (flip (:)) [] xs

-- definujte fciu suciny, ktora pre zoznam cisel vrati rovnakodlhy zoznam cisel
-- ktory obsahuje sucin vsetkych ostatnych cisel zoznamu
-- priklad: suciny [a,b,c,d] = [b*c*d, a*c*d, a*b*d, b*d*d]
suciny :: [Int] -> [Int]
suciny xs = [ product xs `div` x | x <- xs] 
-- vo vasom rieseni nesmiete delit
-- suciny [1..6] = [720,360,240,180,144,120]



main :: IO ()
main = putStrLn "hello"
