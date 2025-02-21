module Main where

import Terms (LExp(..), Var)
--- some useful stuff
import Data.List 
import Data.Char
import Data.Map (Map, insert, lookup, empty)
import Data.Maybe

instance Show LExp where
    show (LAMBDA v b) = "\\" ++ v ++ "->" ++ (show b)
    show (ID v) = v
    show (APP m n) = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
    
--    data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq)

    
-- zoznam vsetkych premennych
vars  :: LExp -> [Var]
vars (LAMBDA v b) = v:(vars b)
vars (APP m n) = (vars m) ++ (vars n)
vars (ID v) = [v]

-- vars isucc = ["n","f","x","f","n","f","x"]
-- nub $ vars isucc = ["n","f","x"]

free :: LExp -> [Var]
free = undefined

subterms :: LExp -> [LExp]
subterms = undefined


hasRedex :: LExp -> Bool
hasRedex (ID v) = False
hasRedex (APP (LAMBDA _ _) _) = True
hasRedex (APP m n) = hasRedex m || hasRedex n
hasRedex (LAMBDA v b) = hasRedex b
--hasRedex _ = False

-- hasRedex  k == False
-- hasRedex  s == False
-- hasRedex  (APP s k) == True
-- hasRedex  (APP (APP s k) k) == False


------------------------------ abstrakcia nad stromovou rekurziou
-- vseobecny lambda traversal pattern

foldLambda :: (Var -> t -> t) -> (Var -> t) -> (t -> t -> t) -> LExp -> t
foldLambda lambda var apl (LAMBDA str exp)  = lambda str (foldLambda lambda var apl exp)
foldLambda lambda var apl (ID str)          = var str
foldLambda lambda var apl (APP exp1 exp2)   = apl (foldLambda lambda var apl exp1) 
                                                  (foldLambda lambda var apl exp2)
                                                  
vars'  :: LExp -> [Var]                                                  
vars' = undefined

show' :: LExp -> String
show' = undefined

hasRedex' :: LExp -> Bool
hasRedex' = undefined

-- hasRedex'  i == False
-- hasRedex'  k == False
-- hasRedex'  s == False
-- hasRedex'  (APP s k) == False   -whoops 
-- hasRedex'  (APP (APP s k) k) == False  -whoops 


parse' :: String -> (LExp, String)
parse' (x:xs) | isAlpha x  = (ID [x], xs)
              | x == '\\' =  let (b, xs') = parse' (drop 3 xs) in ((LAMBDA [head xs] b), xs')                  -- \w->(w w)
              | x == '(' = let (m,(_:xs')) = parse' xs
                               (n, (_:xs'')) = parse' xs'
                           in (APP m n, xs'')                               
     -- (m n)
              | otherwise = error "nieco zle sa stalo" 
              
parse :: String -> LExp
parse s = fst $ parse' s 

--------------------------------- chcelo by to parser lambda termov, aspon primitivny

fromString  :: String -> (LExp, String)
fromString (x:xs) | isAlpha x  = (ID [x], xs)
                  | x == '('  = let (exp1, rest) = fromString xs in
                                  let (exp2, nrest) = fromString (tail rest) in 
                                     (APP exp1 exp2, tail nrest)
                  | x == '\\' = let (exp, rest) = fromString (drop 3 xs) in
                                   (LAMBDA [head xs] exp, rest)
fromString  xs      = error ("syntax error: " ++ xs)   

{- vieme citat po sebe
fromString $ show izero     (\f->\x->x,"")
fromString $ show omega     (\x->(x x),"")                    
fromString $ show isucc     (\n->\f->\x->(f ((n f) x)),"")
-}

-- pozor, readsPrec ocakava nedeterministicky parser, takze ten nas musime dokalicit na zoznam, alebo ten nas prepisat aby vracal zoznam [(LExp, Var)]
instance Read LExp where
  readsPrec _ input = [parse' input]

-- read "(\n->\f->\x->(f ((n f) x))"

-- (read "\\x->x")::LExp  = \x->x
-- (read "\\f->\\x->x")::LExp = \f->\x->x
-- (read "\\x->(x x)")::LExp = \x->(x x)
-- (read "\\n->\\f->\\x->(f ((n f) x))")::LExp = \n->\f->\x->(f ((n f) x))

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


--------------------------------------------
-- type Maybe t = Just t | Nothing
maxim :: Ord t => [t] -> Maybe t
maxim xs = if null xs then Nothing else Just $ maximum xs
minim :: Ord t => [t] -> Maybe t
minim xs = if null xs then Nothing else Just $ minimum xs

rozdielMaxMin :: (Num t, Ord t) => [t] -> Maybe t
rozdielMaxMin xs = let maxi = maxim xs
                       mini = minim xs
                   in if isJust maxi && isJust mini then 
                          Just (fromJust maxi - fromJust mini)
                      else Nothing

-- rozdielMaxMin [5,3,1,8,6,4,2] == Just 7
-- rozdielMaxMin [] == Nothing

-- Maybe Monad style
rozdielMaxMin' :: (Num t, Ord t) => [t] -> Maybe t
rozdielMaxMin' xs = do maxi <- maxim xs
                       mini <- minim xs
                       return (maxi - mini)

-- rozdielMaxMin' [5,3,1,8,6,4,2] == Just 7
-- rozdielMaxMin' [] == Nothing

