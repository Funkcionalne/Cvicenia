-- kodovanie UTF-8 bez BOM (Notepad++)
module TermsDB where
 
-- lambda termy
data LExpDB = LAMBDADB LExpDB | IDDB Int | APPDB LExpDB LExpDB  deriving(Eq)

type SubstDB = [LExpDB]         -- [t0, t1, ..] znamena {0/t0, 1/t1, ... }


instance Show LExpDB where
  show (LAMBDADB e) = -- 'λ' 
                      '\\'  : show e
  show (IDDB v) = show v
  show (APPDB e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
