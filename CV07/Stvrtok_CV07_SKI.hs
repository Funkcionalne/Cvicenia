module Priprava_CV07 where

import Terms
import TermsSKI
--- some useful stuff
import Data.List 
import Data.Char
import Data.Map (Map, insert, lookup, empty, size)
--import Data.Maybe

------------------------------------
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
    
free :: LExpSki -> [Var]
free (Id x) = [x]
free (App x1 x2) = free x1 ++ free x2
free (Lambda x1 x2) = delete x1 (nub(free x2))
free x = []


--vstupný λ-výraz skonvertuje na zodpovedajúci SKI-výraz
toSki :: LExp -> Ski
toSki = lexpSkiToSki . convertSki . lexpToLExpSki
        where
        lexpToLExpSki :: LExp -> LExpSki
        lexpToLExpSki = undefined
        --
        lexpSkiToSki :: LExpSki -> Ski
        lexpSkiToSki = undefined
        --
        convertSki ::  LExpSki -> LExpSki
        convertSki = undefined
  
--vstupný SKI-výraz skonvertuje na λ-výraz
fromSki  ::  Ski -> LExp
fromSki  = lexpSkiToLExp . convertLExp . skiToLExpSki
        where
        lexpSkiToLExp :: LExpSki -> LExp
        lexpSkiToLExp = undefined
        --
        skiToLExpSki :: Ski -> LExpSki
        skiToLExpSki = undefined
        
        --
        convertLExp :: LExpSki -> LExpSki
        convertLExp = undefined
        

--vstupný SKI-výraz zredukuje
nf :: Ski -> Ski
nf l = if a == l then a else nf a where a = oneStep l

oneStep ::  Ski -> Ski
oneStep = undefined

--príklady na vyskúšanie
--1.uloha
a = LAMBDA "x" (LAMBDA "y" (APP (ID "y") (ID "x")))
b = LAMBDA "x" (LAMBDA "y" (APP (ID "x") (ID "y")))
c = LAMBDA "x" (LAMBDA "y" (LAMBDA "z" (APP (APP (ID "x") (ID "y")) (ID "z"))))

--2.uloha
r = APL(APL S K) (APL S K)
s = APL (APL (APL (APL (APL (APL (APL (APL K K) S) K) S) K) K) S) K

du1 = APL (APL (APL (APL (APL (APL (APL S S) K) K) S) K) S) K
du2 = APL (APL (APL (APL (APL (APL (APL (APL (APL S S) K) K) S) K) S) K) S) K
