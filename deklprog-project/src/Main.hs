module Main (main,t,tt,r,p,g) where

import Vars
import Pretty
import Base.Type
import Subst



t :: Term
t = Var (VarName "hi")

tt :: Term
tt = Comb "f" [Comb "1" [], Comb "h" [Comb "g" [Var (VarName "B")], Comb "[]" []]]

r :: Rule
r = Rule t [tt,t,tt,t]

p :: Prog
p = Prog [r,r,r]

g :: Goal
g = Goal [tt,t]


{-

>>>print(allVars t) 

>>>print(allVars tt) 

>>>print(allVars r)

>>>print(allVars p)

>>>print(allVars g)


-}

-- Main function
main :: IO Bool
main =  testSubst