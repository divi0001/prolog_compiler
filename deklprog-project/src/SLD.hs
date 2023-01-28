module SLD
  ( SLDTree (..),
    -- sld,
    -- Strategy,
    -- dfs,
    -- bfs,
    -- solveWith,
  )
where

import Base.Type
import Subst
import Rename
import Unification
import Data.Maybe
import Vars

-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving (Show)

-- sld :: Prog -> Goal -> SLDTree
-- sld (Prog []) g = SLDTree g []
-- sld p (Goal []) = SLDTree (Goal []) []
-- sld (Prog ((Rule r rs):rx) ) (Goal (t:ts)) = if isJust (unify r t) 
--                                              then SLDTree (Goal ts) [(mkUni(unify r t), sld (Prog ((Rule (rename (map allVars r ++ foldl (\ rr -> allVars rr) rs) r) 
--                                              map ((\rrr -> rename (map allVars r ++ foldl (\ rr -> allVars rr) rs) ))rs):rx)) (Goal ts) )] 
--                                              else sld (Prog rx) (Goal (t:ts))




-- mkUni :: Maybe Subst -> Subst
-- mkUni (Just s1) = s1
-- mkUni Nothing = empty