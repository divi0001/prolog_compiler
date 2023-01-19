module Vars

  ( Vars (allVars),
    freshVars,
  )
where
import Base.Type

class Vars a where
  allVars :: a -> [VarName]

instance Vars Term where
  -- allVars :: Term -> [VarName]
  allVars (Var x) = [x]
  allVars (Comb _ []) = []
  allVars (Comb _ list) = notDup (foldl (++) [] (map allVars list))
    where 
      notDup [] = []
      notDup (x:xs) = if notElem x xs then x : notDup xs else notDup xs


-- instance Vars Rule where
  -- allVars (Rule r list) = 
    
    
    
    
    
    
    
    -- concatMap (filter (\r1 -> notElem r1 list) list) list

-- instance Vars where
--   allVars a = helper aTerm []
--     where 
--       helper (Var a) akku         = if not (elem a akku) then [a] else []
--       helper (Comb _ [x:xs]) akku = helper x akku ++ helper xs akku

--still buggy lol

freshVars :: [VarName]
freshVars = [VarName x| let a = ['A' .. 'Z'], let b = ['0' .. '9'], i <- [0 ..], x <- [a !! i ++ b !! i] ]