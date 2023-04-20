module TermsSKI where

data Ski = S | K | I | APL Ski Ski  deriving(Eq) 

instance Show Ski where
    show (S) = "S"
    show (K) = "K"
    show (I) = "I"
    show (APL x y) = "(" ++ show x ++ " " ++ show y ++ ")"

