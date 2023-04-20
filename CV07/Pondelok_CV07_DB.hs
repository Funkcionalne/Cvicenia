module Pondelok_CV07 where

import Terms
import TermsDB
--- some useful stuff
import Data.List 
import Data.Char
import Data.Map (Map, insert, lookup, empty, size)
import Data.Maybe

------------------------------------

maxDepth :: LExp -> Int
maxDepth (ID _) = 0
maxDepth (APP e1 e2) = max (maxDepth e1) (maxDepth e2) 
maxDepth (LAMBDA _ exp) = 1+(maxDepth exp)
---

type Indexes = Map String Int

toDB :: LExp -> LExpDB
toDB term = toDB' 0 term empty

toDB' :: Int -> LExp -> Indexes -> LExpDB
toDB' depth (ID v) m = let vi = Data.Map.lookup v m
                       in if isJust vi then -- viazana premenna
                             IDDB (depth - fromJust vi -1)
                          else -- nenasiel, volna premenna, DU...
                            undefined
toDB' depth (LAMBDA v lexp) m = LAMBDADB (toDB' (depth+1) lexp (Data.Map.insert v depth m))
toDB' depth (APP e1 e2) m =  APPDB (toDB' depth e1 m) (toDB' depth e2 m)


{-           
toDB i = \0
λλ0
λλ1
toDB k = \\1
toDB s = \\\((2 0) (1 0))
-- λz. ((λy. y (λx. x)) (λx. z x))
toDB foo = \(\(0 \0) \(1 0))
-- (λx.λy.((z x) (λu.(u x)))) (λx.(w x))         
toDB goo = (\\((3 1) \(0 2)) \(4 0))
-- λx.λy.y (λz.z x) x
toDB hoo = \\((0 \(0 2)) 1)
-- λx.(λx.x x) (λy.y (λz.x))
toDB ioo = \(\(0 0) \(0 \2))
-}                                    
------------------------------

fromDB :: LExpDB -> LExp
fromDB term = fromDB' term []

--type Indexes' = Map Int String

fromDB' :: LExpDB -> [String] -> LExp
fromDB' (APPDB e1 e2) m  = APP (fromDB' e1 m) (fromDB' e2 m)
fromDB' (IDDB index) m = if (index < length m) then
                               ID (m!!index)
                         else 
                            ID [chr(ord 'a' + index)]
fromDB' (LAMBDADB exp) m = let var = [chr(ord 'x' + length m)]
                           in LAMBDA var (fromDB' exp (var:m))

-----------------------------------
{-
fromDB $ toDB i
\x->x
fromDB $ toDB k
\x->\y->x
fromDB $ toDB s
\x->\y->\z->((x z) (y z))
fromDB $ toDB foo
\x->(\y->(y \z->z) \y->(x y))
fromDB $ toDB goo
(\x->\y->((*** Exception: Prelude.undefined
-}
---------------------------------------                         

subst :: LExpDB -> SubstDB -> LExpDB
subst (IDDB index) sub = sub !! index
subst (APPDB e1 e2) sub = APPDB (subst e1 sub) (subst e2 sub)
subst (LAMBDADB exp) sub = LAMBDADB $ subst exp sub' where sub' = (IDDB 0):map incr sub


incr :: LExpDB -> LExpDB
incr t = t  -- to dodefinovat
        

beta :: LExpDB -> LExpDB -> LExpDB
beta (LAMBDADB m) n = subst m (n:(map (\i -> IDDB i) [0..]))

oneStep :: LExpDB -> LExpDB
oneStep (APPDB (LAMBDADB m) n) = beta (LAMBDADB (oneStep m)) (oneStep n)
oneStep (APPDB m n) = (APPDB (oneStep m) (oneStep n)) 
oneStep (LAMBDADB e) = LAMBDADB (oneStep e)
oneStep t@(IDDB i) = t

nf :: LExpDB -> LExpDB
nf t = if t == t' then t else nf t' where t' = oneStep t 
                         