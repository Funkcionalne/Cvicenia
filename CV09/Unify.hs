module Unify where

import Terms
import Types
import TypeInference

unify  :: Constraints -> Maybe Constraints
unify = undefined

typeExp :: LExp -> Maybe Type
typeExp lterm = case unify (infType lterm) of
                     Just subst -> lookup (Tvar 0) subst
                     Nothing -> Nothing