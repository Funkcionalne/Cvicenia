module Cvicenie05 where

import Data.Map
import Data.Maybe

data Exp = Const Int                -- konstanta
          | Var String                -- premenna
          | Add Exp Exp                -- plus
          | Sub Exp Exp
          | Mul Exp Exp
      deriving (Eq, Read)
      
instance Show Exp where
    show (Const x) = show x
    show (Var x) = x
    show (Add l p)  = "(" ++ (show l) ++ "+" ++ (show p) ++ ")"
    show (Sub l p) = "(" ++ (show l) ++ "-" ++ (show p) ++ ")"
    show (Mul l p) = "(" ++ (show l) ++ "*" ++ (show p) ++ ")"

-- 2x(x-1) + x = 2x^2-x dx = 4x-1
-- (x+x) * (x-1) + x
e0 = (Add (Var "x")(Var "x") )
e1 :: Exp        
e1 = Add 
        (Mul e0 (Sub (Var "x")(Const 1)))
        (Var "x")

-- (x+x) * (x-1)
e2 :: Exp        
e2 = Mul (Add (Var "x") (Var "x")) (Sub (Var "x") (Const 1) )

type Substitucia' = [(String,Exp)]
s' = [("x", Const 2), ("y", Const 6), ("x", Const 7)]

type Substitucia'' = Map String Exp
s'' = insert "x" (Const 7) empty


type Substitucia = String -> Maybe Exp                
s :: Substitucia
s = (\var -> case var of
                                  "x" -> Just $ Const 2
                                  "y" -> Just $ Const 6
                                  otherwise-> Nothing
    )

s2 = \var -> case var of
                "z"  -> Just $ Const 17
                otherwise -> s var
                
insertf :: String -> Exp -> Substitucia -> Substitucia
insertf var e sf = \v -> if v == var then Just e else sf v
               

eval :: Exp -> Substitucia -> Maybe Exp
eval  x@(Const c) s = Just x
eval (Var x) s      = s x

{-
eval (Add l p) s    = let le = eval l s
                          pe = eval p s
                      in if isJust le && isJust pe then
                             Just $ makeAdd (fromJust le) (fromJust pe)
                         else
                            Nothing
-}
eval (Add l p) s    = do le <- eval l s
                         pe <- eval p s
                         return $ makeAdd le pe
eval (Sub l p) s    = do le <- eval l s
                         pe <- eval p s
                         return $ makeSub le pe
eval (Mul l p) s    = do le <- eval l s
                         pe <- eval p s
                         return $ makeMul le pe

              
derive :: Exp -> String -> Exp
derive (Const c) dx = Const 0
derive (Var x) dx = if x==dx then Const 1 else Const 0
derive (Add l p) dx = makeAdd (derive l dx) (derive p dx)
derive (Sub l p) dx = makeSub (derive l dx) (derive p dx)
derive (Mul l p) dx = makeAdd
                        (makeMul (derive l dx) p)
                        (makeMul (derive p dx) l)

-- jemne zjedodusujuci konstruktor suctu
makeAdd :: Exp -> Exp -> Exp
makeAdd (Const c1) (Const c2) = Const (c1+c2)
makeAdd e (Const 0) = e
makeAdd (Const 0) e = e
makeAdd e1 e2 = (Add e1 e2)

-- jemne zjedodusujuci konstruktor rozdielu
makeSub :: Exp -> Exp -> Exp
makeSub (Const c1) (Const c2) = Const (c1-c2)
makeSub e (Const 0) = e
makeSub (Const 0) e = e
makeSub e1 e2 = (Sub e1 e2)

-- jemne zjedodusujuci konstruktor sucinu
makeMul :: Exp -> Exp -> Exp
makeMul (Const c1) (Const c2) = Const (c1*c2)
makeMul e (Const 0) = (Const 0)
makeMul (Const 0) e = (Const 0)
makeMul e (Const 1) = e
makeMul (Const 1) e = e
makeMul e1 e2 = (Mul e1 e2)

simply :: Exp -> Exp
simply  (Add a b) | a==b = Mul (Const 2) a
                  | otherwise = let a' = simply a
                                    b' = simply b
                                in makeAdd a' b'
simply  (Sub a b) = let a' = simply a
                        b' = simply b
                    in makeSub a' b'
simply  (Mul a b) = let a' = simply a
                        b' = simply b
                    in makeMul a' b'
simply x = x


limita :: (Exp -> Exp) -> Exp -> Exp
limita  sim e = let ee = sim e 
                in if ee == e then e else limita sim ee
