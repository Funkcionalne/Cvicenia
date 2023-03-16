module Main where

import Terms (LExp(..), Var)
--- some useful stuff
import Data.List 
import Data.Char
import Data.Map (Map, insert, lookup, empty)
import Data.Maybe
{-
instance Show LExp where
    show (ID var) = var
    show (APP e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (LAMBDA var e) = "\\" ++ var ++ "->" ++ (show e)-}
    
-- zoznam vsetkych premennych
vars  :: LExp -> [Var]
vars (ID var) = [var]
vars (APP e1 e2) = nub ((vars e1) ++ (vars e2))
vars (LAMBDA var e) = nub (var:(vars e))

free :: LExp -> [Var]
free (ID var) = [var]
free (APP e1 e2) = nub ((free e1) ++ (free e2))
free (LAMBDA var e) = (free e) \\ [var]

subterms :: LExp -> [LExp]
subterms = undefined

parse' :: String -> (LExp, String)
parse' (x:xs)
    | isAlpha x = ((ID [x]), xs)
    | x == '(' = let (e1, _:res2) = parse' xs in
                 let (e2, _:res4) = parse' res2 in
                 ((APP e1 e2), res4)
    | x == '\\' = let (v:_:_:ys) = xs in
                  let (e, res) = parse' ys in
                  ((LAMBDA [v] e), res) 
    | otherwise = error "Nieco zle sa stalo"

parse :: String -> LExp
parse = undefined









hasRedex :: LExp -> Bool
hasRedex = undefined

fromString  :: String -> (LExp, String)
fromString (x:xs) | isAlpha x  = (ID [x], xs)
                  | x == '('  = let (exp1, rest) = fromString xs in
                                  let (exp2, nrest) = fromString (tail rest) in 
                                     (APP exp1 exp2, tail nrest)
                  | x == '\\' = let (exp, rest) = fromString (drop 3 xs) in
                                   (LAMBDA [head xs] exp, rest)
fromString  xs      = error ("syntax error: " ++ xs)   

exp1 :: LExp
exp1 = LAMBDA "x" (APP (ID "y") (LAMBDA "z" (ID "m")))

exp2 :: LExp
exp2 = LAMBDA "y" (APP (ID "x") (ID "x"))

exp3 :: LExp
exp3 = APP (ID "x") (LAMBDA "x" (ID "x"))

main :: IO ()
main = do
  print exp1
  print exp2
  print exp3


-- ---------------------------- priklady lambda termov
izero = (LAMBDA "f" (LAMBDA "x" (ID "x")))
omega = (LAMBDA "x" (APP (ID "x") (ID "x")))
isucc = (LAMBDA "n" 
          (LAMBDA "f" 
            (LAMBDA "x" (APP (ID "f") (APP (APP (ID "n") (ID "f")) (ID "x"))) )))
            
-- kominatory S,K,I            
i = (LAMBDA "x" (ID "x"))            
k = (LAMBDA "x" (LAMBDA "y" (ID "x")))            
s = (LAMBDA "x" (LAMBDA "y" (LAMBDA "z" (APP (APP (ID "x") (ID "z")) (APP (ID "y") (ID "z"))))))
            
-- zopar z nich odvodenych konstant            
ione =    (APP isucc izero)
itwo =    (APP isucc (APP isucc izero))
ifour =   (APP isucc (APP isucc (APP isucc (APP isucc izero))))
ieight =  (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc izero))))))))
--ithree =  (APP (APP iplus itwo) ione)
--inine =   (APP (APP itimes ithree) ithree)
--isixteen = (APP (APP ipower itwo) ifour)
