{-
S = \xyz.(xz) (yz)
K = \xy.x
((S K) K) = (((\xyz.(xz) (yz)) K) K)
= ((\yz.(Kz) (yz))  K)
= (\z.(Kz) (Kz))
K a b = a
= (\z.z)
... inak
= (\z.((\xy.x)z) (Kz))
= (\z.((\y.z) (Kz)))
= (\z.(z))
= I

-- omega
(\x.(xx) \x.(xx)) = (\x.(xx) \x.(xx))

\x.((xx)x) \x.((xx)x) = ((\x.((xx)x) \x.((xx)x)) \x.((xx)x))


(\xy.x I)O = \y.I O = I

add m n = \f.\x. m f (n f x)
add \f.\x.(f (f x)) \f.\x.(f (f (f x))) = \f.\x. (\f.\x.(f (f x))) f (f (f (f x)))
add \f.\x.(f (f x)) \f.\x.(f (f (f x))) = \f.\x. (f (f (f (f (f x))))) 

mult m n = \f.\x. n (m f) x

mult m \f.\x.(f(fx)) = \f.\x. n (m f) x
mult m \f.\x.(f(fx)) = \f.\x. (((m f)((m f)x))) 

-----------------
add (succ m) n ?=? succ (add m n)
add (\n.\f.\x.(f (n f x)) m) n =
add (\f.\x.(f (m f x))) n =
(\f.\x. (\f.\x.(f (m f x))) f (n f x))  =
(\f.\x. ((f (m f (n f x)))) )  =
succ (add m n)


exp = \m.\n.(n m)
exp f x = \m.\n.(((n m) f) x



expt = \m -> \n -> n (mult m) one  
expt m n =  two (mult m) one  
expt m n f x =  two (mult m) one  f x
expt m n f x =  (mult m) ((mult m) one)  f x
expt m n f x =  (mult m) (m)  f x
...



decr one ?
one (\m f x -> f (m incr zero))
           zero
           (\x -> x)
           zero 
           =
(\f x -> f (zero incr zero))           
           (\x -> x)
           zero 
= (\x->x) (zero incr zero) =
= zero
----


decr two ?
(two (\m f x -> f (m incr zero))
           zero)
           (\x -> x)
           zero 

two f x = f (f x)

(\m f x -> f (m incr zero)) ((\m f x -> f (m incr zero)) zero)
           (\x -> x)
           zero 

(\m f x -> f (m incr zero)) ((\f x -> f (zero incr zero)) zero)
           (\x -> x)
           zero 

(\f x -> f (((\f x -> f (zero incr zero)) zero) incr zero)) 
           (\x -> x)
           zero 

(\f x -> f (((incr (zero incr zero)) zero) )) 
           (\x -> x)
           zero 

( (((incr (zero incr zero)) zero) )) 
( (((incr zero) zero) )) 




Y = \f ((\x (f (x x))) (\x (f (x x))))
Y F = (\f ((\x (f (x x))) (\x (f (x x))))) F =
((\x (F (x x))) (\x (F (x x)))) =
(F ((\x (F (x x))) (\x (F (x x))))) = F (Y F)





Y = (\f.(\x.(f (x x))) (\x.(f (x x))))
Y F je pevny bod F F x = x
(Y F) = F (Y F)

((\f.(\x.(f (x x))) (\x.(f (x x)))) F) = 
((\x.(F (x x))) (\x.(F (x x)))) =
(F ((\x.(F (x x))) (\x.(F (x x))))) = F (Y F)

H = .....
FAC = Y H = (\f.(\x.(f (x x))) (\x.(f (x x)))) (...)


\x.(x x)




-}
