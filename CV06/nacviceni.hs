
import Terms
import Data.Char
import Data.Maybe

fromString :: String -> (LExp, String)
fromString (a:ax) | a == '\\' = let (bexp,bs) = fromString (drop 3 ax) in (LAMBDA [head ax] bexp, bs)
                  | isAlpha a = (ID [a], ax)
                  | a == '(' = let (bexp, bs) = fromString ax
                                   (cexp, cs) = fromString (tail bs) in ((APP bexp cexp), tail cs)
  
type Subst = [(String,String)]  
  
podobne :: LExp -> LExp -> Maybe Subst 
podobne (ID v1) (ID v2) = Just [(v1,v2)]
podobne (LAMBDA v1 e1) (LAMBDA v2 e2) = let t = podobne e1 e2 in 
                                                if isJust t then (Just ((v1,v2):fromJust t)) 
                                                else Nothing   
podobne (APP e1 e2) (APP e3 e4) = let t1 = (podobne e1 e3)
                                      t2 = (podobne e2 e4) in 
                                      if isJust t1 && isJust t2 then Just ((fromJust t1) ++ (fromJust t2))
                                      else Nothing 
podobne e1 e2 = Nothing      
                                   
                                   