module Terms where
 
-- identifikator premennej je String 
type Var = String

-- lambda termy
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  deriving(Eq, Show)
