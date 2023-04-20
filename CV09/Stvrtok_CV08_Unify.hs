module Stvrtok_CV08_Unify where
import Data.Char
import Data.List

data Term = Var String | 
            CN Int | 
            Functor String [Term]  -- deriving(Show)

-- f(1,2,x), a, f(x,y)
instance Show Term where
    show (Var s) = s
    show (CN n) = show n
    show (Functor f ts) | null ts = f
                        | otherwise = f ++ "(" ++ (intercalate ", " $ map show ts) ++ ")"

e1 = Functor "f" [ Var "X", Var "X" ]               -- f (x, x)
e2 = Functor "f" [ CN 5,    Var "Y" ]               -- f (5, y)
e3 = Functor "f" [ CN 5,    CN 6 ]                  -- f (5, 6)
e4 = Functor "f" [ (Functor "g" [Var "Y"]),    Var "Y" ]                  -- f (g(y), y)
e5 = Functor "f" [ (Functor "g" [Var "Z"]),    Var "Y" ]                  -- f (g(z), y)
e6 = Functor "f" [ (Functor "g" [Var "Z"]),   (Functor "g" [CN 7]) ]      -- f (g(z), g(7))

ee = [
      (Var "U", Functor "f" [Var "V", Var "V"]),
      (Var "Z", Functor "f" [Var "U", Var "U"]),
      (Var "Y", Functor "f" [Var "Z", Var "Z"]),
      (Var "X", Functor "f" [Var "Y", Var "Y"])
      ]

type Constraint = (Term, Term)       -- term1 == term2
type Constraints = [Constraint]      -- zoznam rovnosti
 
unify :: Constraints -> Maybe Constraints
unify ((Functor f1 args1, Functor f2 args2):css) = if f1 == f2 && (length args1) == (length args2) 
                                                then unify((zipWith (,) args1 args2) ++ css)
                                                else Nothing  
unify ((Var s, t):css) = if not (occurs s t) 
                         then add2Maybe (Var s, t)  (unify (substitute'' s t css))  
                         else Nothing
unify ((t, Var s):css) = if not (occurs s t) 
                         then add2Maybe (Var s, t)  (unify (substitute'' s t css))  
                         else Nothing
unify ((Var s, Var e):css) = if s == e then unify css else Nothing
unify ((CN s, CN e):css) = if s == e then unify css else Nothing 
unify [] = Just []
unify css = error ("Erorr :c" ++ (show css))
-- unify [(e1, e2)] = Just [(x,5),(y,5)]
-- unify [(e1, e3)] = Nothing
-- unify [(e1, e4)] = Nothing
-- unify [(e1, e5)] = Just [(x,g(z)),(y,g(z))]
-- unify [(e1, e6)] = Just [(x,g(z)),(z,7)]

occurs :: String -> Term -> Bool
occurs x (Var s) = x == s
occurs x (CN n) = False
occurs x (Functor f args) = any (occurs x) args 
-- or(map (occurs x) args)
-- occurs "x" e1
-- occurs "y" e1

substitute :: String -> Term -> Term -> Term   -- var t t1 = t2, ak t1[x:t]
substitute v t (Var s) | v ==s = t
                       | otherwise = Var s
substitute v t (CN n) = CN n
substitute v t (Functor f args) = Functor f ((map (substitute v t) args)) 
-- substitute "X" (Var "Y") e1

substitute' :: String -> Term -> Constraint -> Constraint
substitute' x t (cs, cs2) = (substitute x t cs, substitute x t cs2)  

substitute'' :: String -> Term -> Constraints -> Constraints
substitute'' x t cs = map (substitute' x t) cs 

-- prida prvok do Maybe List
add2Maybe :: Constraint -> Maybe Constraints -> Maybe Constraints
add2Maybe cs Nothing = Nothing
add2Maybe cs (Just css) = Just (cs:css)   
 
fromString  :: String -> (Term, String)
fromString = undefined

instance Read Term where
    readsPrec _ s = [ fromString s ]

inputs = map (\x -> read x::Term) ["f(X,Y)", "f(g(X),1,Y)", "f(X,f(X,f(X)))", "a", "a(1)"]
