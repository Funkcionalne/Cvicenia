module Analyza where

import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)

-- Newton Raphson vypocet odmocniny (Newtonova metoda pre SQR)
-- a(i+1) = (a(i) + N/a(i))/2

next n old = new where new = (old + n/old)/2

{--
take 100 (iterate (next 2) 10)
"?: " (iterate (next 2) 10)!!100
1.414213562373095
"?: " (iterate (next 2) 999)!!100
1.414213562373095
--}

epsClose  :: Double -> [Double] -> Double
epsClose eps (x:xs@(y:ys)) | abs(x-y) < eps = y
                           | otherwise      = epsClose eps xs
   
sqrt:: Double -> Double -> Double -> Double   
sqrt n init eps = epsClose eps (iterate (next n) init)

epsilon6  :: Double
epsilon6 = 1e-6
mysqrt:: Double -> Double   
mysqrt n = Analyza.sqrt n n epsilon6

qch1 = verboseCheck(\x -> 
             x > 0 ==>
            let s = mysqrt x in abs(s*s-x) < epsilon6)

qch2 = verboseCheck(\x -> 
             x > 0 ==>
            let s = mysqrt x in abs(s - Prelude.sqrt x) < epsilon6)
            
{-
"?: " Analyza.sqrt 2 100 0.0001
1.41421356237384
"?: " Analyza.sqrt 2 100 0.000001
1.414213562373095
"?: " Analyza.sqrt 2 100 0.00000001
1.414213562373095
-}

------------------------------------------------------------
-- Derivacie
numericalDerivation :: (Double -> Double) -> Double -> Double -> Double
numericalDerivation f x eps = (f (x+eps) - f x) / eps

{-
?: " take 10 (iterate (/2) 10)
[10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,7.8125e-2,3.90625e-2,1.953125e-2]
(0.00 secs, 0 bytes)
-}

epsilony  :: [Double]
epsilony = iterate (/2) 10

derivationSequence :: (Double -> Double) -> Double -> [Double]
derivationSequence f x = map (numericalDerivation f x) epsilony

derivation :: (Double -> Double) -> Double -> Double -> Double
derivation f x eps = epsClose eps (derivationSequence f x)

myderivation :: (Double -> Double) -> Double -> Double
myderivation f x = derivation f x epsilon6

qch3 = verboseCheck(\a -> 
            abs(myderivation(\x -> x*x*x) a - 3*a*a) < epsilon6)

{--
"?: " derivation (\x->x*x) 2 0.00001
4.000009536743164
"?: " derivation (\x->x*x) 2 0.0000001
4.000000071525574
"?: " derivation (\x->x*x) 2 0.000000000001
4.0
--}

------------------------------------------------------------
-- Integrovanie

numericalIntegral :: (Double -> Double) -> Double -> Double -> Double
numericalIntegral f a b = (f a + f b)*(b-a) / 2

integralSequence :: (Double -> Double) -> Double -> Double -> [Double]
integralSequence f a b = (numericalIntegral f a b) :
                  (zipWith (+)
                    (integralSequence f a stred)
                    (integralSequence f stred b))
                  where stred = (a+b)/2

integral :: (Double -> Double) -> Double -> Double -> Double -> Double
integral f a b eps = epsClose eps (integralSequence f a b)            

myintegral :: (Double -> Double) -> Double -> Double -> Double
myintegral f a b = integral f a b epsilon6

qch4 = verboseCheck(\a -> 
            abs(myintegral(\x -> x*x*x) 0 a - (a^4)/4) < epsilon6)

{--
"?: " integral (\x->x*x) 0 3 0.0001
9.000017166137695
"?: " integral (\x->x*x) 0 3 0.0000001
9.000000016763806
--}  