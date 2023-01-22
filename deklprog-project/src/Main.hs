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
main =  testSubst --runs all Subst tests

--on my local machine, this line gives me an error:
-- import Text.Parsec hiding (parse)
-- Could not load module ‘Text.Parsec’
-- It is a member of the hidden package ‘parsec-3.1.15.0’.
-- You can run ‘:set -package parsec’ to expose it.
-- (Note: this unloads all the modules in the current scope.)not found

--though this throws no error when compiling, not even a warning and everything runs just fine. Since i am not allowed to edit the Base Folder, ill just leave it like that for now
-- (in Base.Parser.hs)