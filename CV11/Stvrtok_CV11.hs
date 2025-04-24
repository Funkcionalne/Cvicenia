module Stvrtok_CV11_Functor where
import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
import Control.Monad
import Control.Monad.State


data M1 a      = Raise String | Return a  deriving(Show, Read, Eq)
instance Functor M1  where
    fmap  f  (Raise str)     =  Raise str
    fmap  f  (Return x)     =  Return (f x)

{- Cvicenie 1
data M1 a      = Raise String | Return a  deriving(Show, Read, Eq)
instance Functor M1  where
	fmap  f  (Raise str)     =  Raise str
	fmap  f  (Return x)     =  Return (f x)

1)
fmap id ?=? id
- fmap id (Raise str) = Raise str = id (Raise str)
- fmap id (Return x) = Return (id x) = Return x = id (Return x)

2)
fmap (p.q) ?=? (fmap p) . (fmap q)
fmap ((p.q) (Raise str) = Raise str = ((fmap p) . (fmap q)) (Raise str)

L.S. = fmap ((p.q) (Return x) = Return ((p.q) x) = Return (p (q x))
P.S. (fmap p . fmap q) (Return x) = (fmap p) ((fmap q) (Return x)) = 
      (fmap p) (Return (q x)) =
      (Return (p (q x)) = ... L.S. 
      q.e.d.
-}

-- vyhodnotte
w1 = fmap (+1) (Raise "error")
w2 = fmap (+2) (Return 12)
w3 = fmap not (Return True)


x1 = fmap (+1) Nothing
x2 = fmap (*2) (Just 3)
x3 = fmap not (Just False)

-- vyhodnotte  
inc :: (Functor t) => t Int -> t Int
inc = fmap (+1)

x4 = inc (Just 1)
x5 = inc (1,2)  -- ???
x6 = inc [1,2,3,4,5]

-----------------------

data MyMaybe a = MyJust a | MyNothing deriving (Show) -- alias Maybe a
instance Functor MyMaybe  where
       fmap f MyNothing    =  MyNothing
       fmap f (MyJust x)   =  MyJust (f x)

myMaybe1 = MyNothing
myMaybe2 = MyJust "hello"
myMaybe3 = MyJust [1,2,3]

m2 = fmap length myMaybe2
m3 = fmap length myMaybe3       

-------------- Cvicenie 3
       
data LExp a = Var a | APP (LExp a) (LExp a) | ABS a (LExp a) deriving (Show)
instance Functor LExp where
       fmap f (Var x)                   = Var (f x)
       fmap f (APP left right)          = APP (fmap f left) (fmap f right)
       fmap f (ABS x right)             = ABS (f x) (fmap f right)       

omega = ABS "x" (APP (Var "x") (Var "x"))

-- fmap (\x -> x++x) omega

------------------------------------  bin.strom s hodnotami v listoch
data BinTree a = Leaf a | Branch (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

x7 = fmap length (Leaf "abc")
x8 = fmap even (Branch (Leaf 1) (Leaf 2))
x9 = inc (Branch (Leaf 1) (Leaf 2))


----------------------------------------- bin.strom s hodnotami vo vnutornych vrcholoch
data BVS a = Nil | Node (BVS a) a (BVS a) deriving (Show, Eq)
instance Functor BVS where
    fmap f Nil = Nil
    fmap f (Node left key right) = Node (fmap f left) (f key) (fmap f right)

e1 :: BVS Int
e1 = (Node (Node Nil 1 Nil) 2 (Node Nil 1 Nil))
y1 = fmap (+1) e1
y1' = inc e1

e2 :: BVS String
e2 = (Node (Node Nil "1" Nil) "2" (Node Nil "1" Nil))
y2 = fmap (\s -> s++s) e2

instance Arbitrary a => Arbitrary (BVS a) where
  arbitrary = frequency 
              [
                (1, return Nil )
              , (1, liftM3 Node arbitrary arbitrary arbitrary)
              ]
-- generate (arbitrary::Gen (BVS Int))
 
-- overenie miesto dokazu ...              
--qch1 = quickCheck((\t -> fmap id t == t))
qch1 = quickCheck((\t -> fmap id t == t)::BVS String->Bool)

qch2 = quickCheck((\t -> \f -> \g -> fmap (f.g) t == ((fmap f) . (fmap g)) t)
                                    ::BVS Int->(Int->Int)->(Int->Int)->Bool)
-------------------------------------------------------- zoznam
data MyList a = Null | Cons a (MyList a) deriving (Show)       -- [a]
instance  Functor MyList  where
       fmap f Null = Null
       fmap f (Cons x xs) = Cons (f x)(fmap f xs) 

--instance  Functor []  where
fmap' f [] = []
fmap' f (x:xs) = fmap' f xs ++ [f x]
              
y3 = fmap (+1) [1..10]
y4 = fmap' (+1) [1..10]

mylist = (Cons 1 (Cons 2 (Cons 3 Null)))

qch3 = quickCheck((\xs -> fmap id xs == xs)::[Int]->Bool)        
qch4 = quickCheck((\xs -> \f -> \g -> fmap (f.g) xs == fmap f (fmap g xs))
            ::[Int]->(Int->Int)->(Int->Int)->Bool)        
            
qch3' = quickCheck((\xs -> fmap' id xs == xs)::[Int]->Bool)        
qch4' = quickCheck((\xs -> \f -> \g -> fmap' (f.g) xs == fmap' f (fmap' g xs))
            ::[Int]->(Int->Int)->(Int->Int)->Bool)        
    

-------------------------------------------------- Rhododendron
data RoseTree a = Rose a [RoseTree a]    deriving (Show)
instance Functor RoseTree where


--    fmap f (Rose a bs) = Rose (f a) (map (fmap f) bs)
    fmap f (Rose a bs) = Rose (f a) [fmap f s| s<-bs]

r1 :: RoseTree Int
r1 = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 [] ] ]
y5 = inc r1

r2 :: RoseTree [Int]
r2 = Rose [1..3] [Rose [2..5] [], Rose [3] [Rose [4..44] [], Rose [5..55] [] ] ]
y6 = fmap length r2

r3 = Rose "a" [Rose "b" [],Rose "c" [],Rose "d" []]       
y7 = fmap (\x -> x++x) r3

{-
instance Arbitrary a => Arbitrary (RoseTree a) where
 ???
-}

-- =========================================================== Applicative

instance Applicative M1 where
    pure a                  = Return a
    --pure                    = Return
    (Raise e)  <*> _        = Raise e           -- e:: String, Raise e::M1 a
    (Return f) <*> a        = fmap f a

{-
pure a                  = Return a
(Raise e)  <*> _        = Raise e           -- e:: String, Raise e::M1 a
(Return f) <*> a        = fmap f a

potom:
(Return f) <*> (Return x)        = fmap f (Return x) = Return (f x)

------------------------- Cvičenie 5

1) pravidlo: pure id <*> v = v

pure id = Return id
(Return id) <*> v = v
  - (Return id) <*> a = fmap id a = a  , lebo fmap id v = v -- pravidlo identity pre Functors

---

3)
pure f <*> pure x = (Return f) <*> (Return x) = fmap f (Return x) = Return (f x) = pure (f x)

---

2)
L.S.
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
((Return (.)) <*> u) <*> v <*> w = 
(fmap (.) u) <*> v <*> w = 
(fmap (.) (Return fu)) <*> v <*> w = 
(Return ((.) fu)) <*> (Return fv) <*> (Return fw) = 
(Return ((.) fu) fv) <*> (Return fw) = 
(Return (fu . fv) <*> (Return fw) = 
(Return ((fu . fv) fw) 
(Return (fu (fv fw))
= 
R.S. 
Return (fu (fv fw))

---

4) u <*> pure y = (pure $ y) <*> u = pure (\g -> g y) <*> u

(Raise e)
L.S. = (Return f) <*> (Return y) = fmap f (Return y) = Return (f y)
P.S. = (Return ($ y)) <*> (Return f) = fmap ($ y) (Return f) = Return (($ y) f) = Return (f y)
P.S. = (Return (\g -> g y)) <*> (Return f) = Return ((\g -> g y) f) = Return (f y)

-}

ex5 = pure (+) <*> Return 6 <*> Return 9
ex6 = pure (+) <*> Raise "error" <*> Return 9
ex7 = pure (,) <*> Just 1 <*> Just 2
ex8 = pure (\x y z -> (x,y,z)) <*> Just 1 <*> Just 2 <*> Just 3


instance Applicative MyMaybe where
    pure    = MyJust
    --pure f = Just f
    MyNothing <*> _ = MyNothing
    MyJust f <*> a = fmap f a

ex1 = pure (*2) <*> Just 7
ex2 = pure (+) <*> Just 7 <*> Just 9
ex2' = pure (+) <*> MyJust 7 <*> MyJust 9
ex0 = pure (+) <*> Just 6
ex00 = ex0 <*> Just 10


ex3 = pure (+) <*> Nothing <*> Just 9
ex3' = pure (+) <*> MyNothing <*> MyJust 9

ex4 = pure (+) <*> Nothing <*> Nothing

-- pomocny appendik
app Null xs = xs
app (Cons x xs) ys = Cons x (app xs ys)

instance Applicative MyList where
      pure x = Cons x Null    --[x]
      _ <*> Null = Null
      Null <*> _ = Null
      (Cons f fs) <*> (Cons x xs) = app (fmap f (Cons x xs)) (fs <*> (Cons x xs))
            
ee1 = pure (*2) <*> (Cons 1 (Cons 2 (Cons 3 Null)))
ee2 = pure (+) <*> (Cons 1 (Cons 2 (Cons 3 Null))) <*> (Cons 11 (Cons 22 (Cons 33 Null)))
ee3 = pure (,) <*> (Cons 1 (Cons 2 (Cons 3 Null))) <*> (Cons 11 (Cons 22 (Cons 33 Null)))

--    pure x  = [x]
--    fs <*> xs = [ f x| f <- fs, x <- xs]

ee1' = pure (*2) <*> [1,2,3]
ee2' = pure (+) <*> [1,2,3] <*> [11,22,33]
ee3' = pure (,) <*> [1,2,3] <*> [11,22,33]
ee4' = pure (,) <*> [1,2,3] <*> [True, False, True]
ee5' = pure (,) <*> [1,2,3] <*> ['a', 'b', 'c', 'd']

{- Cvicenie 6 overte pravidla pre applicative []

1)  pure id <*> v = v
[id] <*> [x1,...,xn] = [x1,...,xn]   - ocividne

2) LS: (pure (.) <*> [ui]) <*> [vj] <*> [wk] = ([.] <*> [ui]) <*> [vj] <*> [wk] =
        [ui . vj] <*> [wk] = [(ui . vj) wk] = [ui (vj wk)]
   RS: [ui] <*> ([vj] <*> [wk]) = [ui] <*> ([vj wk]) = [ui (vj wk)]
   
3) pure f <*> pure x = pure (f x)
   LS: [f] <*> [x] = [f x]
   RS: [f x]
   
4) u <*> pure y = pure ($ y) <*> u = pure (\g->g y) <*> u
   LS: [ui] <*> [y] = [ui y]
   RS: [\g -> g y] <*> [ui] = [(\g -> g y) ui] = [ui y]
-}


qch33 = quickCheck((\xs -> (pure id <*> xs) == xs)::[Int]->Bool)        
qch34 = quickCheck((\u -> \v -> \w -> 
            (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)))::[Int->Int]->[Int->Int]->[Int]->Bool)        
qch35 = quickCheck((\u -> \v -> \w -> 
            (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)))::[Bool->Int]->[Int->Bool]->[Int]->Bool)        

xx1 = pure (.) <*> [\b -> 5, \b -> 6] <*> [odd, even] <*> [1..10]
xx2 = [odd, even] <*> [1..10]
xx3 = [\b -> if b then 5 else 6] <*> ([odd, even] <*> [1..10])
xx4 = [\b -> if b then 5 else 6, \b -> if b then 15 else 16] <*> ([odd, even] <*> [1..10])

qch36 = quickCheck((\f -> \x -> 
            (pure f <*> (pure x)::[Int]) == pure (f x))::(Int->Int)->Int->Bool)        

qch37 = quickCheck((\u -> \y -> 
            (u <*> pure y) == (pure ($ y) <*> u))::[Int->Int]->Int->Bool)        
