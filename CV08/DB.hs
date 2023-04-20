module DB where

import Terms
import TermsDB
--- some useful stuff
import Data.List hiding (insert, member)
import Data.Char
import Data.Map (Map, insert, lookup, empty, member, (!))
import Data.Maybe -- and maybe not


lDepth :: LExp -> Int
lDepth (ID _) = 0
lDepth (LAMBDA _ e) = 1 + lDepth e
lDepth (APP e1 e2) = max (lDepth e1) (lDepth e2)

maxDepth :: LExp -> Int
maxDepth = lDepth

toDB :: LExp -> LExpDB
toDB term = e
  where (e, _, _) = toDB' term 0 (lDepth term) empty empty

type Depth = Int
type MaxDepth = Int
type IndexMap = Map String Int
type IndexMapGlobal = Map String Int

-- nazvy typov su iba nech sa vam to lahsie cita, kludne si to nahradte podla lubovole
-- d = depth - aktualna hlbka - pre indexovanie viazanych premennych
-- md = maxDepth - pre indexovanie volnych premennych
-- im - indexMap - mapa viazanych premennych
-- img - indexMapGlobal - mapa volnych ("globalnych") premennych


toDB' ::  LExp -> Depth -> MaxDepth ->  IndexMap -> IndexMapGlobal -> (LExpDB, MaxDepth, IndexMapGlobal)
toDB' (ID v) d md im img
  | member v im   = (IDDB (d - im ! v - 1), md, img)
  | member v img  = (IDDB (img ! v), md, img)
  | otherwise     = (IDDB md, md + 1, insert v md img)
toDB' (LAMBDA v e) d md im img = (LAMBDADB e', md', img')
  where (e', md', img') = toDB' e (succ d) md (insert v d im) img
toDB' (APP e1 e2) d md im img = (APPDB e1' e2', md2, img2)
  where (e1', md1, img1) = toDB' e1 d md im img
        (e2', md2, img2) = toDB' e2 d md1 im img1 -- toto je dolezite - vo vypocte pouzivame md1 a img1 z konverzie e1

type Indexes = Map String Int

vars :: [Var]
vars = concatMap (sequence . flip replicate ['a'..'z']) [1..]

fromDB :: LExpDB -> LExp
fromDB dbTerm = undefined
 
subst :: LExpDB -> SubstDB -> LExpDB
subst term subst = undefined

beta :: LExpDB -> LExpDB -> LExpDB
beta dBterm1 dBterm2 = undefined

-- velkonocny bonus
oneStep :: LExpDB -> LExpDB
oneStep (APPDB (LAMBDADB m) n) = beta (LAMBDADB (oneStep m)) (oneStep n)
oneStep (APPDB m n) = APPDB (oneStep m) (oneStep n)
oneStep (LAMBDADB e) = LAMBDADB (oneStep e)
oneStep t@(IDDB i) = t


nf :: LExpDB -> LExpDB
nf t = if t == t' then t else nf t' where t' = oneStep t 

{-
toDB i = \0
toDB k = \\1
toDB s = \\\((2 0) (1 0))
-- foo = λz. ((λy. y (λx. x)) (λx. z x))
toDB foo = \(\(0 \0) \(1 0))
-- goo = (λx.λy.((z x) (λu.(u x)))) (λx.(w x))
toDB goo = (\\((3 1) \(0 2)) \(4 0)) ... voľná premenná
-- hoo = λx.λy.y (λz.z x) x
toDB hoo = \\((0 \(0 2)) 1)
-- ioo = λx.(λx.x x) (λy.y (λz.x))
toDB ioo = \(\(0 0) \(0 \2))

fromDB $ toDB i = \x->x
fromDB $ toDB k = \x->\y->x
fromDB $ toDB s = \x->\y->\z->((x z) (y z))
fromDB $ toDB foo = \x->(\y->(y \z->z) \y->(x y))
fromDB $ toDB goo = (\x->\y->((d x) \z->(z x)) \x->(e x)) ... voľná premenná
fromDB $ toDB hoo = \x->\y->((y \z->(z x)) x)
fromDB $ toDB ioo = \x->(\y->(y y) \y->(y \z->x))
-}


{- examples toDB
toDB i
\0
toDB k
\\1
toDB s
\\\((2 0) (1 0))
toDB foo
\(\(0 \0) \(1 0))
toDB goo
(\\((3 1) \(0 2)) \(4 0))
toDB hoo
\\((0 \(0 2)) 1)
toDB ioo
\(\(0 0) \(0 \2))
toDB izero
\\0
toDB omega
\(0 0)
toDB isucc
\\\(1 ((2 1) 0))
toDB y
\(\(1 (0 0)) \(1 (0 0)))
toDB omega3
\((0 0) 0)
toDB bigOmega
(\(0 0)
toDB ifour
(\\\(1 ((2 1) 0)) (\\\(1 ((2 1) 0)) (\\\(1 ((2 1) 0)) (\\\(1 ((2 1) 0)) \\0))))
toDB iplus
\\\\((3 1) ((2 1) 0))
toDB itimes
\\\\((3 (2 1)) 0)
toDB ipower
\\(0 1)
-}



{- examples fromDB
fromDB $ toDB i
\x->x
fromDB $ toDB k
\x->\y->x
fromDB $ toDB s
\x->\y->\z->((x z) (y z))
fromDB $ toDB foo
\x->(\y->(y \z->z) \y->(x y))
fromDB $ toDB goo
(\x->\y->((d x) \z->(z x)) \x->(e x))
fromDB $ toDB hoo
\x->\y->((y \z->(z x)) x)
fromDB $ toDB ioo
\x->(\y->(y y) \y->(y \z->x))
fromDB $ toDB izero
\x->\y->y
fromDB $ toDB omega
\x->(x x)
fromDB $ toDB isucc
\x->\y->\z->(y ((x y) z))
fromDB $ toDB y
\x->(\y->(x (y y)) \y->(x (y y)))
fromDB $ toDB omega3
\x->((x x) x)
fromDB $ toDB bigOmega
(\x->(x x) \x->(x x))
fromDB $ toDB ifour
(\x->\y->\z->(y ((x y) z)) (\x->\y->\z->(y ((x y) z)) (\x->\y->\z->(y ((x y) z)) (\x->\y->\z->(y ((x y) z)) \x->\y->y))))
fromDB $ toDB iplus
\x->\y->\z->\w->((x z) ((y z) {))
fromDB $ toDB itimes
\x->\y->\z->\w->((x (y z)) {)
fromDB $ toDB ipower
\x->\y->(y x)

-}


{- examples nf
 nf $ toDB ione
\\(1 0)
 nf $ toDB itwo
\\(1 (1 0))
 nf $ toDB ifour
\\(1 (1 (1 (1 0))))
 nf $ toDB iquad
\\(1 (1 (1 (1 0))))
 nf $ toDB ieight
\\(1 (1 (1 (1 (1 (1 (1 (1 0))))))))
 nf $ toDB isix
\\(1 (1 (1 (1 (1 (1 0))))))
 nf $ toDB ithree
\\(1 (1 (1 0)))
 nf $ toDB inine
\\(1 (1 (1 (1 (1 (1 (1 (1 (1 0)))))))))
 nf $ toDB isixteen
\\(1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 (1 0))))))))))))))))
-}
