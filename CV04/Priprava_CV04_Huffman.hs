module Priprava_CV04_Huffman where

import Data.List (group, sort, sortBy)
import Data.Ord

-- silno inspirovane knihou R.Bird, P.Wadler: Introduction to Functional Programming
-- pdf na webe
-- citajte kap. 9.2 Huffman coding trees
-- ALEBO sympaticke videjko s vysvetlenim je tu:  https://www.youtube.com/watch?v=0kNXhFIEd_w

type Weight = Int
type Pair = (Weight, String)
data HTree = Leaf Pair | Node HTree Weight HTree deriving (Show, Eq)
type FreqTable = [Pair]

-- definujte funkciu, ktora vrati frekvencnu tabulku vzostupne usporiadanu podla vah
freqTable   :: String -> FreqTable
freqTable xs = sortBy (comparing fst) $ map (\x -> (length x, [head x])) $ group $ sort xs

--- [(weight_i, char_i)], predpokladame, ze w_1 <= w_2 <= ... w_n
ex1  :: FreqTable
ex1 = [(8,"G"),(9, "R"),(11, "A"),(13,"T"),(17,"E")]

-- chceme porovnavat stromy, podla ich vahy
weight (Leaf (w,_)) = w
weight (Node left weight right) = weight 

instance Ord HTree where
  x < y   = weight x < weight y
  x <= y   = weight x <= weight y
  x > y   = weight x > weight y
  x >= y   = weight x >= weight y
  
build = head . (until single combine) . map Leaf
    where 
      single [x]    = True
      single _      = False

-- vahy su vzdy usporiadane, z prvych dvoch vyrobime stromcek a zaradime do zoznamu taky, aby zostal utriedeny
combine  :: [HTree] -> [HTree]
combine  (x1:x2:xs) = insert (Node x1 ((weight x1) + (weight x2)) x2) xs

-- vseobecny insert-sort, na vstupe mame element a zoznam a chceme ho vhodne vlozit do utriedeneho zoznamu
insert      :: (Ord t) => t -> [t] -> [t]
insert u [] = [u]
insert u (x:xs) | u <= x = u:x:xs
                | otherwise = x : (insert u xs)

tree1 :: HTree
tree1 = build ex1

-- TODO: definujte kodovaciu funkciu vyuzitim charCodes                  
encode  :: HTree -> String -> String
encode tree text = undefined


charCodes :: HTree -> String -> [(String, String)]
charCodes (Leaf (w, s)) path = [(s, path)]
charCodes (Node l w r) path = (charCodes l (path ++ "0")) ++ (charCodes r (path ++ "1"))


hamletHtree = build (freqTable hamlet)                                                                
hamletEncoded = encode hamletHtree hamlet
hamletEncodedLen = (length hamletEncoded) `div` 8
hamletRawLen = length hamlet

-- tato funkcia potom urobi vsetko, freq tabulku, huffmanov strom aj zakoduje
encodeAll :: String -> String
encodeAll xs = encode (build $ freqTable xs) xs

-- TODO: definujte funkciu na dekodovanie retazca {0,1}^* do povodnej spravy
decode  :: HTree -> String ->  String
decode htree str = undefined

hamletDecoded = decode hamletHtree hamletEncoded

-- overte, ci plati let 
-- hamletTree = build (freqTable hamlet) in  decode hamletTree (encode hamletTree hamlet) == hamlet

-- testovaci retazec            
hamlet = "There was this king sitting in his garden all alane " ++
  "When his brother in his ear poured a wee bit of henbane. " ++
  "He stole his brother's crown and his money and his widow. " ++
  "But the dead king walked and got his son and said Hey listen, kiddo! " ++
  " " ++
  "I've been killed, and it's your duty then to take revenge on Claudius. " ++
  "Kill him quick and clean and show the nation what a fraud he is. "  ++
  "The boy said Right, I'll do it. But I'll have to play it crafty. " ++
  "So no one will suspect me, I'll kid on that I'm a dafty."  ++
  " " ++
  "Then with all except Horatio, cuz he counts him as a friend, " ++
  "Hamlet, that's the boy, puts on he's round the bend. " ++
  "But because he was not ready for obligatory killing, " ++
  "He tried to make the king think he was tuppence off a shilling. " ++
  " " ++
  "Got a rise out of Polonius, treats poor Ophelia vile, " ++
  "Told Rosencrantz and Guildenstern Denmark's a bloody jail. " ++
  "Then a troupe of travelling actors, like Seven Eighty-four, " ++
  "Arrived to do a special one night gig in Elsinore. " ++
  " " ++
      "Hamlet! Hamlet! Loved his mommy! " ++
      "Hamlet! Hamlet! Acting barmy! " ++
      "Hamlet! Hamlet! Hesitatin', " ++
      "Wonders if the ghost's a cheat " ++
      "And that is why he's waitin'. " ++
  " " ++
  "Then Hamlet wrote a scene for the players to enact " ++
  "While Horatio and he watched to see if Claudius cracked. " ++
  "The play was called The Mousetrap (not the one that's running now), " ++
  "And sure enough, the king walked out before the final bow. " ++
  " " ++
  "So Hamlet's got the proof that Claudius gave his dad the dose. " ++
  "The only problem being, now, that Claudius knows he knows. " ++
  "So while Hamlet tells his mother her new husband's not a fit one, " ++
  "Uncle Claude puts out a contract with the English king as hit man. " ++
  " " ++
  "And when Hamlet killed Polonius, the concealed corpus delecti, " ++
  "Was the king's excuse to send him for an English hempen necktie. " ++
  "With Rosencrantz and Guildenstern to make sure he got there, " ++
  "Hamlet jumped the boat and put the finger on that pair. " ++
  " " ++
  "Meanwhile Leartes heard his dad had been stabbed through the arras. " ++
  "He came running back to Elsinore, toot-sweet, hotfoot from Paris. " ++
  "And Ophelia, with her dad killed by the man she wished to marry, " ++
  "After saying it with flowers, she committed hari-cari. " ++
  " " ++
      "Hamlet! Hamlet! Nae messin'! " ++
      "Hamlet! Hamlet! Learned his lesson! " ++
      "Hamlet! Hamlet! Yorick's crust " ++
      "Convinced him that men, good or bad, " ++
      "At last must come to dust. " ++
  " " ++
  "Then Leartes lost his cool and was demanding retribution. " ++
  "The king said Keep your head, and I'll provide you a solution."  ++
  "He arranged a sword fight for the interested parties, " ++
  "With a blunted sword for Hamlet, and a sharpened sword for Leartes. " ++
  " " ++
  "To make things double sure, the old belt and braces line, " ++
  "He fixed a poison sword tip, and a poison cup of wine. " ++
  "The poison sword got Hamlet, but Leartes went and muffed it, " ++
  "'Cause he got stabbed himself, and he confessed before he snuffed it. " ++
  " " ++
  "Then Hamlet's mommy drank the wine, and as her face turned blue, " ++
  "Hamlet said I think the king's a baddie through and through."  ++
  "Incestuous, murderous, damned Dane, he said to be precise, " ++
  "And made up for hesitating once by killing Claudius twice, " ++
  " " ++
  "For he stabbed him with the sword and forced the wine between his lips. " ++
  "He cried The rest is silence, and cashed in all his chips. " ++
  "They fired a volley over him that shook the topmost rafter. " ++
  "And Fortinbras, knee-deep in Danes, lived happily ever after. " ++
  " " ++
      "Hamlet! Hamlet! Oh so gory! " ++
      "Hamlet! Hamlet! End of story! " ++
      "Hamlet! Hamlet! I'm away! " ++
      "If you think this is boring, " ++
      "You should read the bloody play. "
            


