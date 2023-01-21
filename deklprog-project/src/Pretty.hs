module Pretty (
  Pretty(pretty)
  ) where

import Base.Type

class Pretty a where
  pretty :: a -> String

{-
>>> pretty (Var (VarName "A"))
"A"

>>> pretty (Comb "true" [])
"true"

>>> pretty (Comb "[]" [])
"[]"

>>> pretty (Comb "f" [Var (VarName "A"), Comb "true" []])
"f(A, true)"

>>> pretty (Comb "f" [Comb "1" [], Comb "h" [Comb "g" [Var (VarName "B")], Comb "[]" []]])
"f(1, h(g(B), []))"

-}

instance Pretty Term where
  pretty (Var (VarName s)) = s
  pretty (Comb s []) = s
  pretty (Comb s ts) = s ++ "(" ++ termString ++ ")"
    where
      termString = intercalate' ", " (map pretty ts)

intercalate' :: [a] -> [[a]] -> [a]
intercalate' sep [] = []
intercalate' sep [x] = x
intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

{-
>>> pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [])
"f(X, true)."

>>> pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")]])
"f(X, true) :- g(X)."

>>> pretty (Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) [Comb "g" [Var (VarName "X")], Comb "h" []])
"f(X, true) :- g(X), h."
-}

instance Pretty Rule where
  pretty (Rule t []) = pretty t ++ "."
  pretty (Rule l rs) = 
    pretty l ++ " :- " ++ intercalate' ", " (map pretty rs) ++ "."

{-

>>> pretty (Prog [])
""

>>> pretty (Prog [Rule (Comb "f" [Var (VarName "X"), Comb "true" []]) []])
"f(X, true)."

>>> pretty (Prog [Rule (Comb "append" [Var (VarName "[]"), Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]])
"append([], Ys, Ys).\nappend(.(X, Xs), Ys, .(X, Zs)) :- append(Xs, Ys, Zs)."

-}

instance Pretty Prog where
  pretty (Prog rs) = intercalate' "\n" (map pretty rs)

{-
>>> pretty (Goal [])

"?- ."

>>> pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []]])
"?- =(X, false)."

>>> pretty (Goal [Comb "=" [Var (VarName "X"), Comb "false" []], Comb "=" [Var (VarName "X"), Comb "true" []]])
"?- =(X, false), =(X, true)."
-}

instance Pretty Goal where
  pretty (Goal ts) = "?- " ++ intercalate' ", " (map pretty ts) ++ "."











{-
>>> pretty empty

"{}"
>>> pretty (single (VarName "A") (Var (VarName "A")))

"{}"
>>> pretty (compose (single (VarName "A") (Var (VarName "A"))) (single (VarName "B") (Var (VarName "B"))))

"{}"
>>> pretty (compose (single (VarName "A") (Var (VarName "B"))) (single (VarName "B") (Var (VarName "A"))))

"{A -> B}"
>>> pretty (compose (single (VarName "A") (Var (VarName "B"))) (single (VarName "A") (Var (VarName "C"))))

"{A -> C}"
>>> pretty (compose (single (VarName "D") (Var (VarName "E"))) (single (VarName "F") (Comb "f" [Var (VarName "D"), Comb "true" []])))

"{F -> f(E, true), D -> E}"
>>> pretty (compose (single (VarName "G") (Var (VarName "H"))) (single (VarName "I") (Var (VarName "J"))))

"{I -> J, G -> H}"
-}

instance Pretty VarName where
  pretty (VarName x) = x