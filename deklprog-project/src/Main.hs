module Main (main,) where

import Vars
import Pretty
import Base.Type
import Subst
import Unification
import Base.Parser
import SLD


{-

>>>print(allVars t) 

>>>print(allVars tt) 

>>>print(allVars r)

>>>print(allVars p)

>>>print(allVars g)


-}
--f(f( ,B))
--f(g(g(),_0))
bigProg :: Prog
bigProg = Prog [
                Rule (Comb "ehemann" [ Var (VarName "christine"), Var (VarName "heinz")]) [],
                Rule (Comb "ehemann" [ Var (VarName "maria"),     Var (VarName "fritz")]) [],
                Rule (Comb "ehemann" [ Var (VarName "monika"),    Var (VarName "herbert")]) [],
                Rule (Comb "ehemann" [ Var (VarName "angelika"),  Var (VarName "hubert")]) [],
                Rule (Comb "ehemann" [ Var (VarName "claudia"),   Var (VarName "karl")]) [],


                Rule (Comb "mutter" [ Var (VarName "herbert"),  Var (VarName "christine")]) [],
                Rule (Comb "mutter" [ Var (VarName "angelika"), Var (VarName "christine")]) [],
                Rule (Comb "mutter" [ Var (VarName "hubert"),   Var (VarName "maria")]) [],
                Rule (Comb "mutter" [ Var (VarName "karl"),     Var (VarName "maria")]) [],
                Rule (Comb "mutter" [ Var (VarName "susanne"),  Var (VarName "monika")]) [],
                Rule (Comb "mutter" [ Var (VarName "norbert"),  Var (VarName "monika")]) [],
                Rule (Comb "mutter" [ Var (VarName "andreas"),  Var (VarName "angelika")]) [],
                Rule (Comb "mutter" [ Var (VarName "anna"),     Var (VarName "claudia")]) [],

                (Rule (Comb "vater" [ Var (VarName "Kind"),Var (VarName "Vater")]) [(Comb "ehemann" [ Var (VarName "Mutter"),Var (VarName "Vater")]), (Comb "mutter" [ Var (VarName "Kind"),Var (VarName "Mutter")])]),

                (Rule (Comb "grossvater" [ Var (VarName "E"),Var (VarName "G")]) [(Comb "vater" [ Var (VarName "E"),Var (VarName "V")]), (Comb "vater" [ Var (VarName "V"),Var (VarName "G")])]),
                (Rule (Comb "grossvater" [ Var (VarName "E"),Var (VarName "G")]) [(Comb "mutter" [ Var (VarName "E"),Var (VarName "M")]), (Comb "vater" [ Var (VarName "M"),Var (VarName "G")])]),

                (Rule (Comb "grossmutter" [ Var (VarName "Person"), Var (VarName "Grossmutter")])
                [(Comb "vater" [ Var (VarName "Person"),Var (VarName "V")]), (Comb "mutter" [ Var (VarName "V"),Var (VarName "Grossmutter")])]),

                (Rule (Comb "grossmutter" [ Var (VarName "Person"), Var (VarName "Grossmutter")])
                [(Comb "mutter" [ Var (VarName "Person"),Var (VarName "M")]), (Comb "mutter" [ Var (VarName "M"),Var (VarName "Grossmutter")])]),

                (Rule (Comb "geschwister" [ Var (VarName "Person"), Var (VarName "Geschwister")]) 
                [(Comb "mutter" [ Var (VarName "Person"), Var (VarName "M")]), (Comb "mutter" [ Var (VarName "Geschwister"), Var (VarName "M")])]),

                (Rule (Comb "geschwister" [ Var (VarName "Person"), Var (VarName "Geschwister")])
                [(Comb "vater" [ Var (VarName "Person"), Var (VarName "M")]), (Comb "vater" [ Var (VarName "Geschwister"),Var (VarName "M")])]),

                (Rule (Comb "tante" [ Var (VarName "Person"), Var (VarName "Tante")])
                [(Comb "ehemann" [ Var (VarName "Tante"), Var (VarName "_")]) , (Comb "mutter" [ Var (VarName "Person"), Var (VarName "M")]), (Comb "geschwister" [ Var (VarName "Tante"), Var (VarName "M")])]),

                (Rule (Comb "tante" [ Var (VarName "Person"), Var (VarName "Tante")])
                [(Comb "ehemann" [ Var (VarName "Tante"), Var (VarName "_")]) , (Comb "vater" [ Var (VarName "Person"), Var (VarName "V")]), (Comb "geschwister" [ Var (VarName "Tante"), Var (VarName "V")])]),

                (Rule (Comb "tante" [ Var (VarName "Person"),Var (VarName "Tante")])
                [(Comb "ehemann" [ Var (VarName "Tante"),Var (VarName "O")]), (Comb "geschwister" [ Var (VarName "O"),Var (VarName "M")]), (Comb "mutter" [ Var (VarName "Person"),Var (VarName "M")])]),

                (Rule (Comb "tante" [ Var (VarName "Person"),Var (VarName "Tante")])
                [(Comb "ehemann" [ Var (VarName "Tante"),Var (VarName "O")]), (Comb "geschwister" [ Var (VarName "O"),Var (VarName "V")]), (Comb "vater" [ Var (VarName "Person"),Var (VarName "V")])])

                ]
p :: Either String a
p = parseFile "D:\\Users\\Riegel\\Desktop\\UNI\\Uni_IDE\\VISUALSTUDIOCODE\\semester_3\\deklProg\\prolog\\8\\8.1.pl"

-- Main function
main :: IO ()
main = print(p)

-- main = print( unify (Comb "f" [Comb "f" [],Var (VarName "B")]) (Comb "f" [Comb "g" [Comb "g" []],Var (VarName "_0")]))
-- main =  testSubst >> testUnification --runs all Subst tests
