module Main (main,t,tt) where

import Vars
import Pretty
import Base.Type



t :: Term
t = Var (VarName "hi")

tt :: Term
tt = Comb "f" [Comb "1" [], Comb "h" [Comb "g" [Var (VarName "B")], Comb "[]" []]]


-- Main function
main :: IO ()
main = print(take 100 freshVars)