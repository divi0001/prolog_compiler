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


instance Vars Rule where
  -- allVars :: Rule -> [VarName]
  allVars (Rule r []) = allVars r
  allVars (Rule r list) = notDup (allVars r ++ concat (map allVars list))


instance Vars Prog where
  allVars (Prog []) = []
  allVars (Prog rlist) = notDup (concat (map allVars rlist))
    

instance Vars Goal where
  allVars (Goal []) = []
  allVars (Goal tlist) = notDup (concatMap allVars tlist)



    


-- freshVars = [VarName x| let a = ['A' .. 'Z'], let b = ['0' .. '9'], i <- [0 ..], x <- [a !! i ++ b !! i] ]
--kept in the commentar i dont know why
zippy :: [Char] -> [String] -> [String] 
zippy [] [] = []
zippy list [] = [list]
zippy [] list = list
zippy list1 (y:list2) = map (:y) list1 ++ zippy list1 list2




freshVars :: [VarName]
freshVars = [VarName x | x <- zippy ['A'..'Z'] ("" : map show [0..])]

notDup :: [VarName] -> [VarName]
notDup [] = []
notDup (x:xs) = if notElem x xs then x : notDup xs else notDup xs