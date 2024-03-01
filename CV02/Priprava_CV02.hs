module Priprava_CV02 where
import Test.QuickCheck
import Text.Show.Functions
import Data.List


-- pocet 3906
-- slova dlzky 3 nad abecedou
slova3 :: [Char] -> [String]
slova3 abeceda = [[ch1,ch2,ch3] | ch1 <- abeceda, ch2 <- abeceda, ch3 <- abeceda]

qchSlova3 = quickCheck (\abeceda -> length abeceda < 12 ==> length (slova3 abeceda)== let p = length abeceda in p*p*p)


-- slova dlzky k nad abecedou
-- slova :: [Char] -> Int -> [String]
slova :: String -> Int -> [String]
slova abeceda 0 = [ [] ]   -- lebo dlzky 0 je jedine slovo, a to je \epsilon
slova abeceda n = [ ch:w | ch <- abeceda, w <- slova abeceda (n-1) ]   -- dovod bol na prednaske

-- slova "01" 3 = ["000","001","010","011","100","101","110","111"]
-- slova dlzky k nad abecedou su vlastne variacie s opakovanim

pocetSlova :: [Char] -> Int -> Int 
pocetSlova abeceda k = (length abeceda) ^ k

qchSlova = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && length abeceda < 6) 
                    ==> length (slova abeceda k) == pocetSlova abeceda k)

qchSlova' = quickCheck(\abeceda -> \kk -> (length abeceda < 6) 
                    ==> let k = kk `mod` 10 in 
                        length (slova abeceda k) == pocetSlova abeceda k)

-- slova dlzky najviac k nad abecedou, zle riesenie
-- slova dlzky najviac k
slovaNajviac :: [Char] -> Int -> [String]
slovaNajviac abeceda 0  = [[]]
slovaNajviac abeceda k = slovaNajviac abeceda (k-1) ++ 
                         [ ch:w | w <-slovaNajviac abeceda (k-1), ch <- abeceda]

--length $ slovaNajviac "ABCDEF" 2 = 49 != 1+6+36 = 43
--slova' 2 = ["","A","B","C","D","E","F","A","B","C","D","E","F","AA","BA","CA","DA","EA","FA","AB","BB","CB","DB","EB","FB","AC","BC","CC","DC","EC","FC","AD","BD","CD","DD","ED","FD","AE","BE","CE","DE","EE","FE","AF","BF","CF","DF","EF","FF"]

--slovaNajviac "ABCDEF" 2 = 43
--BTW. kolko je 1+6+36+...+6^k (slova dlzky najviac k) ?


pocetSlovaNajviac [] k = 1
--pocetSlovaNajviac [_] k = k+1
pocetSlovaNajviac abeceda k = (size^(k+1)-1) `div` (size-1) where size = length abeceda
-- explicit "ABCDEF" 2 = 43

qchSlovaNajviac = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && 
                                     1 < length abeceda && length abeceda < 6 &&
                                     length abeceda == length ( nub abeceda)) ==> 
                    length ( nub (slovaNajviac abeceda k)) == pocetSlovaNajviac abeceda k)

qchNajviacABCDEF = quickCheck(\kk -> let k = kk `mod` 7 in
                    length ( nub (slovaNajviac "abcdef" k)) == pocetSlovaNajviac "abcdef" k)


-- dobre riesenie
slovaNajviac' abeceda k = concat [ slova abeceda dlzka | dlzka <- [0..k] ]
-- length $ slovaNajviac' "abcdef" 2

qchNajviac' = quickCheck(\abeceda -> \k -> (k >= 0 && k <= 10 && 
                                     1 < length abeceda && length abeceda < 6 &&
                                     length abeceda == length ( nub abeceda)) ==> 
                    length ( slovaNajviac' abeceda k) == pocetSlovaNajviac abeceda k)




-- slova nad abecedou "ab", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA :: Int -> [String]
slovaBezAA 0 = [ [] ]
slovaBezAA 1 = [ "a", "b" ]
slovaBezAA k =  [  'b':s | s <-slovaBezAA (k-1) ] 
                ++
                [  'a':'b':s | s <- slovaBezAA (k-2) ] 

slovaBezAAFilter n = filter (not.isInfixOf "aa") $ slova "ab" n
qchSlovaBezAA = quickCheck(\k -> k >= 0 && k <= 10 ==>
                            (length (slovaBezAAFilter k) == fibonacci k))

                
fibonacci 0 = 1
fibonacci 1 = 2
fibonacci k = fibonacci (k-1) + fibonacci (k-2)

qchSlovaBezAAFilter = quickCheck(\k -> k >= 0 && k <= 10 ==>
                            (length (slovaBezAA k) == fibonacci k))

-- slova nad abecedou "a...", ktore neobsahuju aa, teda dve acka za sebou
slovaBezAA' :: String -> Int -> [String]
slovaBezAA' abeceda 0 = [ [] ]
slovaBezAA' abeceda 1 = [ [ch] | ch <- abeceda ]
slovaBezAA' abeceda k = [  ch:s | s <- slovaBezAA' abeceda (k-1), ch <- abeceda, ch /= 'a' ] 
                        ++
                        [  'a':ch:s | s <- slovaBezAA' abeceda (k-2), ch <- abeceda, ch /= 'a' ] 


fibonacci' abeceda 0 = 1
fibonacci' abeceda 1 = length abeceda
fibonacci' abeceda k = (length abeceda-1) * (fibonacci' abeceda (k-1) + fibonacci' abeceda (k-2))
                
qchSlovaBezAA' = quickCheck(\k -> k >= 0 && k <= 10 ==>
                                    (length (slovaBezAA' "abcd" k) == fibonacci' "abcd" k))


slovaBezAAFilter' abeceda n = filter (not.isInfixOf "aa") $ slova abeceda n

qchSlovaBezAA'Filter = quickCheck(\k -> k >= 0 && k <= 10 ==>
                                    (length (slovaBezAAFilter' "abcd" k) == fibonacci' "abcd" k))



{- ako vybehnut na skody vysky n, ked mozem robit kroky dlzky 1,2
n = 4 => 5
0|1,2,3|4       bbb
0|1,2, |4       bba
0|1,  3|4       bab
0|  2,3|4       abb
0|  2, |4       aba

n = 5 => 8
0|1,2,3,4|5     bbbb
0|1,2,3, |5     bbba
0|1,2,  4|5     bbab
0|1,  3,4|5     babb
0|1,  3, |5     baba
0|  2,3,4|5     abbb
0|  2,3, |5     abba
0|  2,  4|5     abab
0|  2,3,4|5     abbb

n = 6 => 13....
-}
-- izomorfizmus - zobrazime slovo neobsahujce dve posebe iduce a na riesenie schody
izoAA2Schody :: String -> [Int]
izoAA2Schody w = [ index |(ch,index)<-zip w [1..], ch == 'b' ]

aa2schody n = map izoAA2Schody $ slovaBezAA (n-1)

izoSchody2AA :: Int -> [Int]->String
izoSchody2AA n sch = [ if elem index sch then 'b' else 'a' | index <-[1..n] ]

schody2AA n = map (izoSchody2AA (n-1)) schody
                where schody = aa2schody n
-- map (izoSchody2AA 4) $ aa2schody 5

-- definujte predikat pre usporiadany/rastuci/nerastuci zoznam
ordered :: [Int] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:xs@(y:ys)) = x<=y && ordered xs

ordered' :: [Int] -> Bool
ordered' [] = True
ordered'  xs = all (\(x,y) -> x <= y) $ zip (init xs) (tail xs)


-- definujte a overte nejaku vlastnost funkcie ordered

qch1 = quickCheck( \xs -> ordered xs == ordered' xs )
qch1v = verboseCheck(\xs -> ordered $ sort xs  )
qch1v' = verboseCheck(\xs -> ordered' $ sort xs  )

qch1r = verboseCheck(\xs -> (length xs > 1) ==> not $ ordered $ reverse $ sort xs  )

-- naprogramuje jeden prechod bubble sort algoritmu
bubble :: [Int] -> [Int]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) = if x>y then y:(bubble(x:xs)) else x:(bubble(y:xs))

-- pouzite bubble na bubbleSort
bubbleSort  :: [Int]->[Int]
bubbleSort xs = (iterate bubble xs)!!(length xs)
e = bubbleSort [4,3,4,6,7,4,3,1,1,2,3,4,5,6,7,8,9,0,5,3,2,3,2,3,4,5,6,7,1,2,2,0,9,12,11]
--[0,0,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,6,7,7,7,8,9,9,11,12]

-- definujte a overte nejaku vlastnost funkcie bubbleSort
qch2 = quickCheck( \xs -> bubbleSort xs == sort xs)
qch2v = undefined -- verboseCheck( \xs -> cond ==> proposition )

powerSet :: [t] -> [[t]]
powerSet [] = [[]]
powerSet (x:xs) = let ps = powerSet xs in ps ++ [x:ys | ys <- ps]

qch3 = quickCheck((\xs -> (length xs < 10) ==> (length $ powerSet xs) == (2^(length xs)))::[Int]->Property)

qch3' = verboseCheck((\xs -> (length xs < 10) ==> 
        (length $ nub $ powerSet xs) == (2^(length xs)))::[Int]->Property)

qch3'' = verboseCheck((\n -> (n >= 0 && n < 10) ==> 
        (length $ nub $ powerSet [1..n]) == (2^n))::Int->Property)

subset :: (Eq t) => [t] -> [t] -> Bool
subset xs ys = and [ elem x ys | x<-xs]

subset' :: (Eq t) => [t] -> [t] -> Bool
subset' xs ys = all  (\x -> elem x ys) xs

qch3''' = quickCheck((\n -> (n >= 0 && n < 10) ==> 
                        and [ subset m [1..n] | m <- powerSet [1..n] ])::Int->Property)

---------------------------------
-- kompozícia zoznamu funkcií,  :: [a->a] -> (a->a)

-- zaciatocnicka definicia cez zoznamovu rekurziu
kompozicia  :: [a->a] -> (a->a)
kompozicia [] = id
kompozicia (f:fs) = (\x -> f (kompozicia fs x))

-- definicia haskellistu, co si nasiel operator $
kompozicia''  :: [a->a] -> (a->a)
kompozicia'' [] = id
kompozicia'' (f:fs) = \x -> f $ kompozicia'' fs x

qchf = quickCheck((\f -> \g -> \x -> (kompozicia [f,g]) x == (kompozicia'' [f,g]) x)::
            (Int->Int) -> (Int->Int) -> Int -> Bool)

-- definicia haskellistu, co si este prehodil x na lavu stranu
kompozicia'''''  :: [a->a] -> (a->a)
kompozicia''''' [] x      = x
kompozicia''''' (f:fs) x  = f $ kompozicia''''' fs x

-- jemne pokrocily haskellista, ktory bol na prednaske
kompozicia'  :: [a->a] -> (a->a)
kompozicia' [] = id
kompozicia' (f:fs) = f . kompozicia' fs

-- haskellista, co si pamata, ze skladanie funkcii je asociativne ale nepamata, ze nie je komutativne
kompozicia''''  :: [a->a] -> (a->a)
kompozicia'''' [] = id
kompozicia'''' (f:fs) = kompozicia'''' fs . f

-- haskellista, co bude volit lavicu
kompoziciaLeft  :: [a->a] -> (a->a)
kompoziciaLeft = foldl (.) id

-- haskellista, co bude volit neexistujucu pravicu
kompoziciaRight  :: [a->a] -> (a->a)
kompoziciaRight = foldr (.) id

zoznamfcii = [(+7),(*11),(`mod` 1234567),(`div` 10),(^4),(+1),(*2),(^3)]

{-
*Main> kompozicia      zoznamfcii 1
95
*Main> kompozicia''    zoznamfcii 1
95
*Main> kompozicia''''' zoznamfcii 1
95
*Main> kompozicia'     zoznamfcii 1
95
*Main> kompozicia''''  zoznamfcii 1
550158565384
*Main> kompoziciaLeft  zoznamfcii 1
95
*Main> kompoziciaRight zoznamfcii 1
95

-- kompozicia funkcii nie je komutativna

*Main> kompozicia      (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''    (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''''' (reverse zoznamfcii) 1
550158565384
*Main> kompozicia'     (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''''  (reverse zoznamfcii) 1
95
*Main> kompoziciaLeft  (reverse zoznamfcii) 1
550158565384
*Main> kompoziciaRight (reverse zoznamfcii) 1
550158565384

-- evidentne definicia kompozicia'''' je zla, kedze predpokladala komutativnost (.)
-}
