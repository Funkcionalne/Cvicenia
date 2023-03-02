--                          1,4,7,..   2,5,8,...
kazdyTretiPrec :: [t] -> ([t],     [t])
kazdyTretiPrec xs =  let (_, mod1, mod2) = foldl (\(i, as, bs) -> \x -> if i `mod` 3 == 0 then (i+1, as, bs)
                                                              else if i `mod` 3 == 1 then (i+1, x:as, bs)
                                                              else (i+1, as, x:bs)
                                            ) (0,[],[]) (reverse xs)
                     in ( mod1,  mod2)                       

kazdyTretiPrec' :: [t] -> ([t],     [t])
kazdyTretiPrec' xs =  let (_, mod1, mod2) = foldr ( \x -> \(i, as, bs) -> 
                                                              if i `mod` 3 == 0 then (i-1, as, bs)                            
                                                              else if i `mod` 3 == 1 then (i-1, x:as, bs)
                                                              else (i-1, as, x:bs)
                                                  )
                                                  (length xs-1,[],[]) xs
                      in ( mod1,  mod2)                       

pn   :: [Int] -> Int
pn  xs =  sum as - sum bs  
            where (as, bs) = foldr (\x -> \(as, bs) -> (bs, x:as)) ([],[]) xs

pn'   :: [Int] -> Int
pn'  xs =  a - b  
            where (a, b) = foldr (\x -> \(a, b) -> (b, x+a)) (0,0) xs
            
pn''   :: [Int] -> Int
pn''  xs = negate $ foldr (-) 0 xs
            

