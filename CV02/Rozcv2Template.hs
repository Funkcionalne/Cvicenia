import Data.List
import Test.QuickCheck

-- slova nad abecedou dlzky k, kde susedne pismenka su rozne
-- Priklad: 
{-
slovaSusedneRozne "abc" 4 = ["baba","caba","acba","bcba","baca","caca","abca","cbca","abab","cbab","acab","bcab","bacb","cacb","abcb","c
bcb","abac","cbac","acac","bcac","babc","cabc","acbc","bcbc"]

slovaSusedneRozneFilter  "ab" 5 = ["ababa","babab"]
slovaSusedneRozneFilter  "ab" 6 = ["ababab","bababa"]
slovaSusedneRozneFilter  "ab" 7 = ["abababa","bababab"]

slovaSusedneRozneFilter  "abcde" 2 = ["ab","ac","ad","ae","ba","bc","bd","be","ca","cb","cd","ce","da","db","dc","de","ea","eb","ec","ed"]
-}

-- rekurzia/list-comprehension
slovaSusedneRozne :: String -> Int -> [String]
slovaSusedneRozne abeceda k = undefined      ------------- <============================ tu dopis nieco

-- brutte force = filter cez vsetky mozne slova - treba vyhadzat tie, v ktorych sa opakuje aa, alebo bb, alebo ...
slovaSusedneRozneFilter :: String -> Int -> [String]
slovaSusedneRozneFilter abeceda k = undefined ------------- <============================ tu dopis nieco

-- z cvicenia, pouzite
slova :: String -> Int -> [String]
slova abeceda 0 = [ [] ] 
slova abeceda n = [ ch:w | ch <- abeceda, w <- slova abeceda (n-1) ] 

-- nejaka nakodena formula, co spocita pocet rieseni
pocetSlovaSusedneRozne :: String -> Int -> Int
pocetSlovaSusedneRozne abeceda k = undefined  ------------- <============================ tu dopis nieco

-- quickCheck 1. vs. 3.
qchSlovaSusedneRozne = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && 
                                   1 < length abeceda && length abeceda < 6 &&
                                   length abeceda == length (nub abeceda)) ==> -- test, ktory diskvalifikuje abecedy s rovnakymi znakmi
                                   
                                   slova abeceda k == slova abeceda k  -- tento riadok je cely zle, je tu len preto, aby template isiel skompilovat, cely ho nahradte testom, ktory testuje riesenie 1. vs. vzorec 3.
                                   
                                   )


-- quickCheck 2. vs. 3.
qchSlovaSusedneRozneFilter = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && 
                                     1 < length abeceda && length abeceda < 6 &&
                                     length abeceda == length (nub abeceda)) ==> -- test, ktory diskvalifikuje abecedy s rovnakymi znakmi
                                     
                                     slova abeceda k == slova abeceda k  -- tento riadok je cely zle, je tu len preto, aby template isiel skompilovat, cely ho nahradte testom, ktory testuje riesenie 2. vs. vzorec 3.
                                     
                                     )
