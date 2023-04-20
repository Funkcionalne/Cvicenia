module SKI where

import Terms
import TermsSKI
--- some useful stuff
import Data.List 
import Data.Char
import Data.Map (Map, insert, lookup, empty, size)
import Data.Maybe

toSki :: LExp -> Ski
toSki exp = undefined

fromSki :: Ski -> LExp
fromSki ski = undefined

oneStep ::  Ski -> Ski
oneStep = undefined

-- medzi forma
data LExpSki = Lambda String LExpSki | Id String | App LExpSki LExpSki | Ss | Kk | Ii
                deriving(Eq)                
    
instance Show LExpSki where
    show (Id x) = x
    show (App x1 x2) = "(" ++ show x1 ++ ") (" ++ show x2 ++ ")"
    show (Lambda x1 x2) = "\\" ++ x1 ++ ".(" ++ show x2 ++ ")"
    show (Ss) = "S"
    show (Kk) = "K"
    show (Ii) = "I"                

-- normalizator ako velkonocny darcek
nf :: Ski -> Ski
nf l = if a == l then a else nf a where a = oneStep l

{- examples
toSki a
((S (K (S I))) ((S (K K)) I))
toSki b
((S ((S (K S)) ((S (K K)) I))) (K I))
toSki c
((S ((S (K S)) ((S (K (S (K S)))) ((S (K (S (K K)))) ((S ((S (K S)) ((S (K K)) I))) (K I)))))) (K (K I)))
toSki r

fromSki r
((\x->\y->\z->((x z) (y z)) \x->\y->x) (\x->\y->\z->((x z) (y z)) \x->\y->x))

fromSki s
((((((((\x->\y->x \x->\y->x) \x->\y->\z->((x z) (y z))) \x->\y->x) \x->\y->\z->((x z) (y z))) \x->\y->x) \x->\y->x) \x->\y->\z->((x z) (y z))) \x->\y->x)

nf du1
K
nf du2
S
-}
