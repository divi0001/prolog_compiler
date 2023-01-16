module Vars

  ( -- Vars (allVars),
    -- freshVars,
  )
where
import Base.Type

-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   foldl (+) 0 [1,2,3]

class Vars where

  allVars :: a -> [VarName]

-- instance Vars where
--   allVars a = helper aTerm []
--     where 
--       helper (Var a) akku         = if not (elem a akku) then [a] else []
--       helper (Comb _ [x:xs]) akku = helper x akku ++ helper xs akku

-- instance Vars Term where
--   -- allVars :: Term -> [VarName]
--   allVars (Var x) = [x]
--   allVars (Comb _ []) = []
--   allVars (Comb _ list) = foldl (++) [] (map (allVars)  (list))