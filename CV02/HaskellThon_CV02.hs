import Data.List 


-- vyhod kazdy n-ty prvok z pola
dropEvery :: [Int] -> Int -> [Int]
dropEvery xs n = [x | (x, pos) <- (zip xs [1..]), pos `mod` n /= 0]  
--dropEvery xs n = map snd $ filter (\(i,x) -> i `mod` n /= 0) $ zip [1..] xs
{-dropEvery [] _ = []
dropEvery xs n = (take (n-1) xs ) ++ dropEvery (drop n xs) n -}
-- dropEvery xs n = foldl (\acc (x, pos) -> (if pos `mod` n /= 0 then acc ++ [x] else acc)) [] (zip xs [1..(length xs)]) -- vynechali sme

-- rozdel zoznam na dva, prvy ma dlzku n
split :: [Int] -> Int -> ([Int], [Int])
split xs n = (take n xs, drop n xs)
--split xs n = splitAt n xs
{-split [] _ = ([], [])
split xs 0 = ([], xs)
split xs n = ((head xs):ys, zs)  
              where
                (ys, zs) = split (tail xs) (n-1)-}
{-split (x:xs) n | n > 0 = let (ys, zs) = split xs (n-1) in (x:ys, zs) -- vynechali sme
split xs _ = ([], xs) -} 

-- vrat prvky listu medzi indexami i a j (vratane)
slice :: [Int] -> Int -> Int -> [Int]
slice xs i j = drop i (take j xs)
--slice xs i j = map (xs!!) [i..j]

-- check ci je postupnost graficka
--graficka :: [Int] -> Bool
{-graficka xs | all (==0) xs = True
            | any (<0) xs = False
            | otherwise = let (y:ys) = reverse (sort xs ) in (y <= length ys) && graficka ( (map (subtract 1) (take y ys)) ++ (drop y ys) ) -}

{- graficka xs | elem (-1) xs = False
            | sum xs == 0 = True
            | otherwise = (y <= length ys) && ( graficka  $ (map (\x -> x-1) $ take y ys) ++ (drop y ys))
                where
                (y:ys) = reverse (sort xs) -}



------------------------------------------
-- naprogramujte vlastny zip -> na vstupe su dva zoznamy, na vystupe zoznam dvojic (polymorfne); 
zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = [(xs!!i, ys!!i) | i <- [0..(len-1)] ]
            where 
              len = minimum [(length xs), (length ys)]
--  where n = min (length xs) (length ys)              

-- vlastny zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = let len = min (length xs) (length ys) in [f (xs!!i) (ys!!i) | i <- [0..(len-1)] ]    

-- zip 3 zoznamov
zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' xs ys zs = [(xs!!i, ys!!i, zs!!i) | i <- [0..(len-1)] ]
            where 
              len = minimum [(length xs), (length ys), (length zs)]

-- zip n zoznamov integerov, na vstupe je pole poli a na vystupe tiez (pre zjednodusenie mame na vystupe list miesto tuple)
zipN :: [[Int]] -> [[Int]] 
zipN xss = let len = minimum (map length xss) in [[xs!!i | xs <- xss] | i <- [0..(len-1)]]

