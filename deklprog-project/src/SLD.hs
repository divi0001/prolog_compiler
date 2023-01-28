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
import Data.List

-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving (Show)

sld :: Prog -> Goal -> SLDTree
sld (Prog []) g = SLDTree g []
sld p (Goal []) = SLDTree (Goal []) []
sld p g = sldHelp p g [] 

sldHelp :: Prog -> Goal -> [VarName] -> SLDTree
sldHelp (Prog []) g vars = SLDTree g []
sldHelp p (Goal []) vars = SLDTree (Goal []) []
sldHelp (Prog [Rule r rs]) (Goal [t]) vars = if isJust (unify r t) then SLDTree (Goal []) [(mkUni(unify r t), sldHelp (Prog [rename vars (Rule r rs)]) (Goal []) (vars ++ allVars (Rule r rs)))] else SLDTree (Goal [t]) []
sldHelp (Prog ((Rule r rs):rx)) (Goal (t:ts)) vars =  if isJust (unify r t)
                                                        then SLDTree (Goal ts) [(mkUni(unify r t), sldHelp (Prog (renameList vars (Rule r rs:rx))) (Goal ts) (vars ++ allVars (Rule r rs)))] 
                                                        else sld (Prog (Rule r rs:rx)) (Goal (t:ts))


renameList :: [VarName] -> [Rule] -> [Rule]
renameList vars r = map (rename vars) r 

mkUni :: Maybe Subst -> Subst
mkUni (Just s1) = s1
mkUni Nothing = empty