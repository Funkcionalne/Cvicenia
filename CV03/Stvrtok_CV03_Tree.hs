module Utorok_CV03_Tree where
import Test.QuickCheck
import Tree

{-
data BVS t = Nil | Node (BVS t) t (BVS t) deriving(Show, Eq)
-}

-- napiste konstatu typu BVS Int
e' :: BVS Int
e' = Node (Node Nil 1 Nil) 5 (Node Nil 7 Nil)

-- velkost stromu je pocet Node uzlov
size :: BVS t -> Int
size Nil = 0
size (Node a b c) = 1 + size a + size c


-- napiste tvrdenie, ze ak x sa nachadza v strome t prave vtedy ak x sa nachadza v zozname flat t 



qch2P = undefined --quickCheck((\x -> \tree -> ..... ::Int->BVS Int->Bool)


-- ak je isBVS, potom find x tree je ako hladanie x v splostenom zozname
qch4 = quickCheckWith stdArgs{ maxSuccess = 100000 } ((\x -> \tree -> (isBVS tree) ==> ((find x tree) == (elem x (flat tree))))::Int->BVS Int->Property)         

insert :: (Ord t) => t -> BVS t -> BVS t
insert x Nil = Node Nil x Nil
insert x t@(Node l v r) 
    | x == v = t
    | x < v = Node (insert x l) v r
    | otherwise = Node l v (insert x r)

-- napiste tvrdenie, ze ak vsunieme prvok do stromu, jeho velkost sa zvacsi o 1...
qOnSize = quickCheck((\x -> \t -> isBVS t && not ( find x t) ==> size t + 1 == size  (insert x t) )::Int->BVS Int->Property)


-- e = Node Nil 4 (Node Nil 7 Nil)                                                       
-- insert 1 e = Node (Node Nil 1 Nil) 4 (Node Nil 7 Nil)
-- insert 5 e = Node Nil 4 (Node (Node Nil 5 Nil) 7 Nil)
-- insert 9 e = Node Nil 4 (Node Nil 7 (Node Nil 9 Nil))

-- x sa po inserte urcite v strome nachadza
-- qch5 = quickCheckWith stdArgs{ maxSuccess = 100000 } 


-- velkost stromu po inserte je o jedna vacsia, asi neplati, ak sa x tam uz nachadza
-- qch6 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ...  )
-- Failed! Falsifiable (after 1 test):

-- ak sa x v strome nenachadza a insertneme ho tam, tak bude o 1 vacsi
-- qch7 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ...  )

-- aj ked ho tam 2x insertneme, strom sa zvacsi len o 1
-- qch8 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ... )

-- maximalny v strome, ale musi byt isBVS
maxBVS                        :: BVS t -> t
maxBVS Nil              = error "something wrong"  -- error :: String -> t
maxBVS (Node _ val Nil) = val
maxBVS (Node left val right) = maxBVS right  

-- delete v strome, ale musi byt isBVS
delete :: (Ord t) => t -> BVS t -> BVS t
delete = undefined

{-                                
e = Node Nil 4 (Node Nil 7 Nil)
delete 4 e = Node Nil 7 Nil
delete 7 e = Node Nil 4 Nil
delete 1 e = Node Nil 4 (Node Nil 7 Nil)
-}

-- ak sa x nachadza v strome, po delete bude o jeden uzol mensi                             
-- qch9 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ... )

-- ak sa x nenachadza v strome po delete
-- qch10 = quickCheckWith stdArgs{ maxSuccess = 100000 }( ... )

          
u = Node Nil 4 (Node Nil 7 Nil)                
u1 = Node Nil 4 (Node Nil 4 Nil)                

-- v strome su len rovnake hodnoty                                
isUnival :: (Eq t) => BVS t -> Bool  
isUnival = undefined


same [] = True
same (x:xs) = all ( == x) xs

isUnival' tree = same(flat(tree))

qOnUnival = quickCheck((\tree -> isUnival tree == isUnival' tree)
        ::BVS Int->Bool)
        
        
