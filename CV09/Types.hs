module Types where

data Type = Tvar Integer | Type :-> Type  deriving (Eq)

type Context = [(String, Type)]

type Constraint = (Type,Type)
type Constraints = [Constraint]

instance Show Type where
    show (Tvar i) = "T" ++ show i
    show (t1 :-> t2) = "(" ++ show t1 ++ "->" ++ show t2 ++ ")"
