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
-- bigProg :: Prog
-- bigProg = Prog [
--                 Rule (Comb "ehemann" [ Var (VarName "christine"), Var (VarName "heinz")]) [],
--                 Rule (Comb "ehemann" [ Var (VarName "maria"),     Var (VarName "fritz")]) [],
--                 Rule (Comb "ehemann" [ Var (VarName "monika"),    Var (VarName "herbert")]) [],
--                 Rule (Comb "ehemann" [ Var (VarName "angelika"),  Var (VarName "hubert")]) [],
--                 Rule (Comb "ehemann" [ Var (VarName "claudia"),   Var (VarName "karl")]) [],


--                 Rule (Comb "mutter" [ Var (VarName "herbert"),  Var (VarName "christine")]) [],
--                 Rule (Comb "mutter" [ Var (VarName "angelika"), Var (VarName "christine")]) [],
--                 Rule (Comb "mutter" [ Var (VarName "hubert"),   Var (VarName "maria")]) [],
--                 Rule (Comb "mutter" [ Var (VarName "karl"),     Var (VarName "maria")]) [],
--                 Rule (Comb "mutter" [ Var (VarName "susanne"),  Var (VarName "monika")]) [],
--                 Rule (Comb "mutter" [ Var (VarName "norbert"),  Var (VarName "monika")]) [],
--                 Rule (Comb "mutter" [ Var (VarName "andreas"),  Var (VarName "angelika")]) [],
--                 Rule (Comb "mutter" [ Var (VarName "anna"),     Var (VarName "claudia")]) [],

--                 (Rule (Comb "vater" [ Var (VarName "Kind"),Var (VarName "Vater")]) [(Comb "ehemann" [ Var (VarName "Mutter"),Var (VarName "Vater")]), (Comb "mutter" [ Var (VarName "Kind"),Var (VarName "Mutter")])]),

--                 (Rule (Comb "grossvater" [ Var (VarName "E"),Var (VarName "G")]) [(Comb "vater" [ Var (VarName "E"),Var (VarName "V")]), (Comb "vater" [ Var (VarName "V"),Var (VarName "G")])]),
--                 (Rule (Comb "grossvater" [ Var (VarName "E"),Var (VarName "G")]) [(Comb "mutter" [ Var (VarName "E"),Var (VarName "M")]), (Comb "vater" [ Var (VarName "M"),Var (VarName "G")])]),

--                 (Rule (Comb "grossmutter" [ Var (VarName "Person"), Var (VarName "Grossmutter")])
--                 [(Comb "vater" [ Var (VarName "Person"),Var (VarName "V")]), (Comb "mutter" [ Var (VarName "V"),Var (VarName "Grossmutter")])]),

--                 (Rule (Comb "grossmutter" [ Var (VarName "Person"), Var (VarName "Grossmutter")])
--                 [(Comb "mutter" [ Var (VarName "Person"),Var (VarName "M")]), (Comb "mutter" [ Var (VarName "M"),Var (VarName "Grossmutter")])]),

--                 (Rule (Comb "geschwister" [ Var (VarName "Person"), Var (VarName "Geschwister")]) 
--                 [(Comb "mutter" [ Var (VarName "Person"), Var (VarName "M")]), (Comb "mutter" [ Var (VarName "Geschwister"), Var (VarName "M")])]),

--                 (Rule (Comb "geschwister" [ Var (VarName "Person"), Var (VarName "Geschwister")])
--                 [(Comb "vater" [ Var (VarName "Person"), Var (VarName "M")]), (Comb "vater" [ Var (VarName "Geschwister"),Var (VarName "M")])]),

--                 (Rule (Comb "tante" [ Var (VarName "Person"), Var (VarName "Tante")])
--                 [(Comb "ehemann" [ Var (VarName "Tante"), Var (VarName "_")]) , (Comb "mutter" [ Var (VarName "Person"), Var (VarName "M")]), (Comb "geschwister" [ Var (VarName "Tante"), Var (VarName "M")])]),

--                 (Rule (Comb "tante" [ Var (VarName "Person"), Var (VarName "Tante")])
--                 [(Comb "ehemann" [ Var (VarName "Tante"), Var (VarName "_")]) , (Comb "vater" [ Var (VarName "Person"), Var (VarName "V")]), (Comb "geschwister" [ Var (VarName "Tante"), Var (VarName "V")])]),

--                 (Rule (Comb "tante" [ Var (VarName "Person"),Var (VarName "Tante")])
--                 [(Comb "ehemann" [ Var (VarName "Tante"),Var (VarName "O")]), (Comb "geschwister" [ Var (VarName "O"),Var (VarName "M")]), (Comb "mutter" [ Var (VarName "Person"),Var (VarName "M")])]),

--                 (Rule (Comb "tante" [ Var (VarName "Person"),Var (VarName "Tante")])
--                 [(Comb "ehemann" [ Var (VarName "Tante"),Var (VarName "O")]), (Comb "geschwister" [ Var (VarName "O"),Var (VarName "V")]), (Comb "vater" [ Var (VarName "Person"),Var (VarName "V")])])

--                 ]

-- p = parseFile "D:\\Users\\Riegel\\Desktop\\UNI\\Uni_IDE\\VISUALSTUDIOCODE\\semester_3\\deklProg\\prolog\\8\\8.1.pl"

-- Main function

ha8 :: Prog
ha8 = Prog [Rule (Comb "person" [Comb "herrmann" []]) [],Rule (Comb "person" [Comb "voigt" []]) [],Rule (Comb "person" [Comb "davidova" []]) [],Rule (Comb "person" [Comb "samuelsson" []]) [],Rule (Comb "person" [Comb "boe" []]) [],Rule (Comb "person" [Comb "lien" []]) [],Rule (Comb "person" [Comb "laegreid" []]) [],Rule (Comb "team" [Comb "nor" []]) [],Rule (Comb "team" [Comb "ger" []]) [],Rule (Comb "team" [Comb "swe" []]) [],Rule (Comb "team" [Comb "cze" []]) [],Rule (Comb "wettkampf" [Comb "verfolgungf" []]) [],Rule (Comb "wettkampf" [Comb "verfolgungm" []]) [],Rule (Comb "wettkampf" [Comb "sprint" []]) [],Rule (Comb "nimmtTeilAn" [Comb "herrmann" 
      [],Comb "sprint" []]) [],Rule (Comb "nimmtTeilAn" [Comb "voigt" [],Comb "verfolgungf" []]) [],Rule (Comb "nimmtTeilAn" [Comb "davidova" [],Comb "sprint" []]) [],Rule (Comb "nimmtTeilAn" [Comb "samuelsson" [],Comb "verfolgungm" []]) [],Rule (Comb "nimmtTeilAn" [Comb "boe" [],Comb "verfolgungm" []]) [],Rule (Comb "nimmtTeilAn" [Comb "lien" [],Comb "verfolgungf" []]) [],Rule (Comb "nimmtTeilAn" [Comb "laegreid" [],Comb "verfolgungm" []]) [],Rule (Comb "laeuftFuer" [Comb "herrmann" [],Comb "ger" []]) [],Rule (Comb "laeuftFuer" [Comb "voigt" [],Comb "ger" []]) [],Rule (Comb "laeuftFuer" [Comb "davidova" [],Comb "cze" []]) [],Rule (Comb "laeuftFuer" [Comb "samuelsson" [],Comb "swe" []]) [],Rule (Comb "laeuftFuer" [Comb "boe" [],Comb "nor" []]) [],Rule (Comb "laeuftFuer" [Comb "lien" [],Comb "nor" []]) [],Rule (Comb "laeuftFuer" [Comb "laegreid" [],Comb "nor" []]) [],Rule (Comb "laufenZusammen" [Var (VarName "Person1"),Var (VarName "Person2")]) [Comb "laeuftFuer" [Var (VarName "Person1"),Var (VarName "X")],Comb "laeuftFuer" [Var (VarName "Person2"),Var (VarName "X")]],Rule (Comb "konkurriertMit" [Var (VarName "Person1"),Var (VarName "Person2")]) [Comb "nimmtTeilAn" [Var (VarName "Person1"),Var (VarName "X")],Comb "nimmtTeilAn" [Var (VarName "Person2"),Var (VarName "X")]]]

call :: Goal
call = Goal [Comb "laeuftFuer" [Var (VarName "X"),Comb "ger" []]]
-- call = Goal [Comb "laeuftFuer" [Comb "herrmann" [],Comb "swe" []]]


-- main :: IO (Either String Goal)
-- main = parseFile "D:\\Users\\Riegel\\Desktop\\UNI\\Uni_IDE\\VISUALSTUDIOCODE\\semester_3\\deklProg\\prolog\\8\\8.pl"


-- main = print( unify (Comb "f" [Comb "f" [],Var (VarName "B")]) (Comb "f" [Comb "g" [Comb "g" []],Var (VarName "_0")]))
-- main =  testSubst >> testUnification --runs all Subst tests

main :: IO()
main = print (solveWith ha8 call bfs)