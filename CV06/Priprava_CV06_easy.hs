module Cvicenie06 where
import Unsafe.Coerce -- kvoli tazkym trikom, radsej netusit...

-----------------------------------------------

-- zdroj http://techtipshoge.blogspot.sk/2011/06/church-number-with-ski-combinators.html
-- http://www.angelfire.com/tx4/cus/combinator/birds.html

-- SKI combinator  
-- ?x.x  
i = \x -> x

-- ?xy.x  
k = \x -> \y -> x

-- ?xyz.x z (y z)  
s = \x -> \y -> \z -> x z (y z)
-- s = \x -> \y -> \z -> ((x z) (y z))

-- inak to je church's one
apply'   = s (s k)                            -- ?xy.x y 
apply''  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
apply'''  = i

-- Church's numeral  
-- ?fx.x
zero  = k i  
-- ?fx.(f x)
one   = i  
one'  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
two   = (s (s (k s) k)) i  
three = (s (s (k s) k)) (s (s (k s) k) i)  
four  = (s (s (k s) k)) ((s (s (k s) k)) (s (s (k s) k) i))  


-- For assertion only  
type Church = (Int -> Int) -> Int -> Int  
  
-- integer -> church  
church n = \f -> \x -> iterate f x !! n  

ch_add = \m -> \n -> \f -> \x -> m f (n f x)     

-- one  
ch_one = \f -> \x -> f x  
  
-- n++  
ch_inc = \n -> \f -> \x -> f (n f x)  
  
-- m*n  
ch_mult = \m -> \n -> \f -> \x -> m (n f) x  
  
-- m^n  
ch_expt = \m -> \n -> n (ch_mult m) one  
   

unChurch n = n (+1) (0)  
            

a = church 2  
b = church 10   
  
main = do print $ unChurch $ ch_add a b  
          print $ unChurch $ ch_inc a  
          print $ unChurch $ ch_mult a b  
          print $ unChurch $ ch_expt a b 
          print $ unChurch $ i a
          print $ unChurch $ k a b
          print $ unChurch zero  
          print $ unChurch one  
          print $ unChurch one'
          print $ unChurch two  
          print $ unChurch three  
          print $ unChurch four  
          print $ unsafeApply apply' (+1) 10   
          print $ unsafeApply apply'' (^2) 10   
          print $ unsafeApply apply''' (subtract 99) 10  
          
unsafeApply n a b = unsafeApply' (unsafeCoerce n) a b            
      where unsafeApply' :: Church -> (Int -> Int) -> Int -> Int  
            unsafeApply' n a b = n a b  
          