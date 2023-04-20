module Priprava_CV08_Unify where
import Data.Char

data Term = Var String | 
            CN Int | 
            Functor String [Term]  deriving(Show)
            
instance Read Term where
    readsPrec _ s = [ fromString s ]
    
{-
instance Show Term where
    show (CN n) = show n
    show (Var name) = name
    show (Functor f args) = f ++ if null args then [] 
                                 else 
                                   "(" ++ (show (args!!0))++ (concat [ "," ++ (show (args!!i))  | i <- [1..length args-1] ]) ++ ")" 
-}


type Constraint = (Term, Term)       -- term1 == term2
type Constraints = [Constraint]

unify :: Constraints -> Maybe Constraints
unify [] = Just []
unify (((Functor f fArgs),(Functor g gArgs)):cs) = 
                          if f == g && length fArgs == length gArgs then 
                            unify ([(fArgs!!i, gArgs!!i) | i <- [0..length fArgs-1]] ++ cs)
                          else Nothing
unify ((Var vName, t):cs) = if occurs vName  t then Nothing else add2Maybe (Var vName, t) (unify (substitute'' vName t cs))
unify ((t, Var vName):cs) = if occurs vName  t then Nothing else add2Maybe (Var vName, t) (unify (substitute'' vName t cs))
unify ((CN a, CN b):cs) = if a /= b then Nothing else Just cs
unify ((_, _):cs) = Nothing

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

inputs = map (\x -> read x::Term) ["f(X,Y)", "f(g(X),1,Y)", "f(X,f(X,f(X)))", "a", "a(1)"]

-- unify [(e1, e2)] = Just [(x,5),(y,5)]
-- unify [(e1, e3)] = Nothing
-- unify [(e1, e4)] = Nothing
-- unify [(e1, e5)] = Just [(x,g(z)),(y,g(z))]
-- unify [(e1, e6)] = Just [(x,g(z)),(z,7)]

occurs :: String -> Term -> Bool
occurs name (CN _) = False
occurs name (Var vName) = name == vName
occurs name (Functor f args) = any (occurs name) args
-- occurs "x" e1
-- occurs "y" e1

substitute :: String -> Term -> Term -> Term   -- var t t1 = t2, ak t1[x:t]
substitute name t t1@(CN i) = t1
substitute name t t1@(Var vName) = if name == vName then t else t1
substitute name t (Functor f args) = (Functor f [substitute name t a | a<-args])
-- substitute "x" (Var "y") e1

substitute' :: String -> Term -> Constraint -> Constraint
substitute' name t (c1, c2) = (substitute name t c1, substitute name t c2)

substitute'' :: String -> Term -> Constraints -> Constraints
substitute'' name t cs = [ substitute' name t c | c <- cs]

-- prida prvok do Maybe List
add2Maybe :: Constraint -> Maybe Constraints -> Maybe Constraints
add2Maybe a (Just ls) = Just (a:ls) 
add2Maybe _ Nothing   = Nothing
 

fromStringArgs  :: String -> ([Term], String)
fromStringArgs xs = let (a, xs') = fromString xs in 
                       if not (null xs') &&  head xs' == ',' then let (as,xs'') = fromStringArgs (tail xs') in ((a:as), xs'')
                       else ([a], tail xs')


fromString  :: String -> (Term, String)
fromString (x:xs)   | isDigit x  = (CN (ord x-48), xs)
fromString [x]      | isAlpha x && isLower x  = (Functor [x] [], [])
fromString (x:y:xs) | isAlpha x && isLower x && y == '(' = let (args, xs') = fromStringArgs xs in (Functor [x] args, xs')
fromString (x:xs)   | isAlpha x && isUpper x  = (Var [x], xs)
fromString  xs      = error ("syntax error: " ++ xs)                                                   
