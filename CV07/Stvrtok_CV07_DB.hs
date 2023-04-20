module Stvrtok_CV07 where

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
maxDepth (LAMBDA _ e) = 1 + maxDepth e
---


toDB :: LExp -> LExpDB
toDB term = toDB' 0 term empty

type Indexes = Map String Int
toDB' :: Int -> LExp -> Indexes -> LExpDB
toDB' n (LAMBDA v e) d = LAMBDADB (toDB' (n+1) e (Data.Map.insert v n d))
toDB' n (APP e1 e2) d = APPDB (toDB' n e1 d) (toDB' n e2 d)
toDB' n (ID v) d = let res = Data.Map.lookup v d in
                   if isJust res then
                      let i = fromJust res in IDDB (n-i-1)
                   else undefined

e = LAMBDA "x" (APP (ID "x") (ID "x"))
y1 = LAMBDA "x" (APP (ID "f") (APP (ID "x") (ID "x")))
y2 = LAMBDA "f" (APP y1 y1)

fromDB

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
fromDB term = 

--type Indexes' = Map Int String
-- fromDB' :: LExpDB -> Indexes' -> LExp

fromDB' :: LExpDB -> [String] -> LExp
fromDB' = undefined

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
subst (IDDB k) s = s !! k
subst (APPDB e1 e2) s = (APPDB (subst e1 s) (subst e2 s))
subst (LAMBDADB e) s = (LAMBDADB subst e (0: (map incr s))) 

incr :: LExpDB -> LExpDB
incr t = t  -- to dodefinovat
        

beta :: LExpDB -> LExpDB -> LExpDB
beta (LAMBDADB m) n = undefined

oneStep :: LExpDB -> LExpDB
oneStep = undefined

nf :: LExpDB -> LExpDB
nf t = if t == t' then t else nf t' where t' = oneStep t 
                         