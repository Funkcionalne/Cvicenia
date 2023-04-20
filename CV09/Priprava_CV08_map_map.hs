{--
"?: " :type map
map :: (a -> b) -> [a] -> [b]

vieme otypovat vyraz map map ?

-- typ map map?
-- typ map :: (a->b)->[a]->[b]
-- zoberme si napriklad f::a->Bool
-- typ map f :: [a]->[Bool]
-- tak ze ked chceme typ map map, musime do typu map dosadit za funkciu unarnu funkciu.
-- typ map :: (a'->b')->[a']->[b']  -- aby sa nam nepoplietli typy, premenovali sme ich
-- takze typ map ako unarnej funkcie je (a'->b')->([a']->[b'])
-- a mozeme dosadit do typu map za a dame (a'->b') a za b dame ([a']->[b']), 
-- nezabudnite, ze po dosadeni prvy argument vo vysledom type map vynechavame
-- dostaneme map map :: [(a'->b')]->[[a']->[b']] a mozeme premenovat bez ciarok
-- filter p = concat . map f
-- where f x = ... doplnte
--     f x = if p x then [x] else []

MAP map
MAP :: (A -> B) -> [A] -> [B]
map :: (a -> b) -> ([a] -> [b])
preto A = (a->b), B = [a] -> [b]
MAP :: ((a->b) -> ([a] -> [b])) -> [a->b] -> [[a] -> [b]]
MAP map :: [a->b] -> [[a] -> [b]]

nech a = b = Int
MAP :: ((Int->Int) -> ([Int] -> [Int])) -> [Int->Int] -> [[Int] -> [Int]]

(map map) [(+1),(+2),(*3)]
nevypise, lebo je to zoznam funckcii

length $ (map map) [(+1),(+2),(*3)]
3

(map map) [(+1),(+2),(*3)]!!0 [1..10]

"?: " ((map map) [(+1),(+2),(*3)]!!0) [1..10]
[2,3,4,5,6,7,8,9,10,11]

"?: " ((map map) [(+1),(+2),(*3)]!!2) [1..10]
[3,6,9,12,15,18,21,24,27,30]
--}

