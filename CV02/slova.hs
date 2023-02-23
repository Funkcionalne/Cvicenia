import Data.List
import Test.QuickCheck

slova :: Int -> [String]
slova 0 = [""]
slova n = [x:w | w<-slova (n-1), x <-['0','1']]

slovaBez00 n = filter (not.isInfixOf "00") $ slova n

pocetBez00 n = length $ slovaBez00 n
pocetBez00' n = fib n 1 1
                where fib 0 a b = b
                      fib n a b = fib (n-1) b (a+b)
                      
-- take 20 $ map snd (iterate (\(a,b) -> (b, a+b)) (1,1))                      
pocetBez00'' n = snd $ (iterate (\(a,b) -> (b, a+b)) (1,1))!!n

pocetBez00''' n = 2^n - (n-1)*2^(n-2)

                      
hypoteza1' = quickCheck( \n -> (n > 0 && n <= 20) ==> pocetBez00 n == pocetBez00' n)
hypoteza1'' = quickCheck( \n -> (n > 0 && n <= 20) ==> pocetBez00 n == pocetBez00'' n)
hypoteza1''' = quickCheck( \n -> (n > 0 && n <= 20) ==> pocetBez00 n == pocetBez00''' n)
                      
slovaBez000 n = filter (not.isInfixOf "000") $ slova n
pocetBez000 n = length $ slovaBez000 n
pocetBez000' n | n == 0 = 1
               | n == 1 = 2
               | n == 2 = 4
               | otherwise =   pocetBez000' (n-1)+ -- 1...  
                               pocetBez000' (n-2)+ -- 01... 
                               pocetBez000' (n-3)  -- 001... 

pocetBez000'' n = res 
       where (res,_,_) =(iterate (\(a,b,c) -> (b, c, a+b+c)) (1,2,4))!!n

hypoteza2 = quickCheck( \n -> (n > 0 && n <= 20) ==> pocetBez000 n == pocetBez000' n)
hypoteza2' = quickCheck( \n -> (n > 0 && n <= 20) ==> pocetBez000 n == pocetBez000'' n)