module TypeInference where

import Terms
import Types

infType t = inferType [(t, Tvar 0)] [] [] 1

inferType :: [(LExp,Type)]->Context->Constraints->Int->Constraints
inferType = undefined