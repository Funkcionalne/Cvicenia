
-- CHURCH'S NUMERAL  
-- ?FX.X
ZERO  = K I  
-- ?FX.(F X)
-- ONE  
ONE = \F -> \X -> F X  
ONE''   = I  
--ONE''' = (S (S K))
ONE'  = (S ( (S (K S)) ((S (K K) I)) )) (K I)
TWO   = \F -> \X -> F (F X)
TWO'   = (S (S (K S) K)) I  
THREE = (S (S (K S) K)) (S (S (K S) K) I)  
FOUR  = (S (S (K S) K)) ((S (S (K S) K)) (S (S (K S) K) I))  

incr = \n -> \f -> \x -> f (n f x)     
incr' n = \f -> \x -> f (n f x)     

add = \m -> \n -> \f -> \x -> m f (n f x)     
add' m n = \f -> \x -> m f (n f x)     
  
-- inak to je church's one
apply'   = s (s k)                            -- ?xy.x y 
apply''  = (s ( (s (k s)) ((s (k k) i)) )) (k i)
apply'''  = i
----------------------------------------------------  
-- zero
\f.\x.x
\f.I = K I
---

\f.\x.(f x)
\f.(S (\x.f) (\x.x)) =
\f.((S (K f)) I) =
S (\f.(S (K f)) \f.I) =
S (\f.(S (K f)) (K I)) =
S (S (\f.S \f.(K f)) (K I)) =
S (S ((K S) \f.(K f)) (K I)) =
S (S ((K S) S (\f.K \f.f)) (K I)) =
S (S ((K S) S ((K K) I)) (K I)) a b = a(b)

I a b = a(b)

S (S K) a b = ((S K) b) (a b) = (K (a b)) (b (a b)) = a b





















X = \x.(x K S K)
X = \x.(((x K) S) K)

K ? = (X X) X
S ? = X (X X)
-------
(X X) X = (\x.(x K S K) X) X =
(X K S K)X =
(\x.(x K S K) K S K)X =
((K K S K) S K) X =
((K K) S K) X =
(K K) X =
K
------
\x.(x K S K) (X X) =
((X X) K S K)  =
((\x.(x K S K) X) K S K)  =
((X K S K) K S K)  =
(((K K S K) S K) K S K)  =
(((K K) S K) K S K)  =
(K S K)  =
S









