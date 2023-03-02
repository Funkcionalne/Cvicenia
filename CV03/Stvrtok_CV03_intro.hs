module Stvrtok_CV_intro where

-- 5x5
tab = ["ABCDE",
       "FGHIJ",
       "KLMNO",
       "PQRST",
       "UVWXY"
    ] 
    
-- nájdite všetky slová začínajúce A, končiace Y, ak môžeme ísť leb vpravo a dole
-- koľko ich je
slova :: Int -> Int -> [String]
slova i j 
    | i == 4 && j == 4 = ["Y"]
    | i > 4 || j > 4 = []
    | otherwise = [tab !! i !! j : x | x <- slova i (j+1)] ++ [tab !! i !! j : x | x <- slova (i+1) j]


-- skuste to prepisat do list comprehension, resp. do map
slova' :: Int -> Int -> [String]
slova' i j = undefined

-- kolko ich je ?

zlozitost :: Int -> Int -> Int
zlozitost = undefined




















-- MxN = comb( (M-1) + (N-1), (M-1))
--  comb( 2*(M-1) , (N-1))
-- MxN = comb( (M-1) + (N-1), (M-1))
--  comb( 2*(M-1) , (N-1))

-- (n/2).... n / (1....(n/2)) = 2^(n/2)

-- slova, ktore obsahuju Q

obsahujuQ :: [String]
obsahujuQ = undefined

-- slova, ktore neobsahuju Q

neobsahujuQ :: [String]
neobsahujuQ = undefined

{-
tab = ["ABCDE",
       "FGHIJ",
       "KLMNO",
       "PQRST",
       "UVWXY"
       

-}
