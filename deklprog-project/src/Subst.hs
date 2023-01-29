{-#LANGUAGE TemplateHaskell#-}

module Subst
  ( Subst, -- don't export the constructor of the data type!
    domain,
    empty,
    single,
    compose,
    apply,
    restrictTo,
    testSubst,
    isEmpty,
  )
where

import Base.Type
import Data.List
import Data.Typeable
import Test.QuickCheck
import Vars
import Pretty

-- Data type for substitutions
data Subst = Subst [(VarName, Term)]
  deriving (Show,Eq)


-- Generator for substitutions
instance Arbitrary Subst where
  -- We use the `suchThat` combinator to filter out substitutions that are not valid,
  -- i.e. whose domain contains the same variable more than once.
  arbitrary = Subst <$> (arbitrary `suchThat` ((\vts -> length vts == length (nub vts)) . map fst))

-- Properties

-- Uncomment this to test the properties when all required functions are implemented

-- Applying the empty substitution to a term should not change the term
prop_1 :: Term -> Bool
prop_1 t = apply empty t == t

-- Applying a singleton substitution {X -> t} to X should return t
prop_2 :: VarName -> Term -> Bool
prop_2 x t = apply (single x t) (Var x) == t

-- Applying a composed substitution is equal to applying the two substitutions individually
prop_3 :: Term -> Subst -> Subst -> Bool
prop_3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

-- Comb "g" [Var (VarName "B"),Comb "f" []]
-- Subst [(VarName "B",Var (VarName "B")),(VarName "_0",Var (VarName "_0"))]
-- Subst [(VarName "_1",Var (VarName "_1")),(VarName "_0",Var (VarName "A"))]

-- The domain of the empty substitution is empty
prop_4 :: Bool
prop_4 = null (domain empty)

-- The domain of a singleton substitution {X -> X} is empty
prop_5 :: VarName -> Bool
prop_5 x = null (domain (single x (Var x)))

-- The domain of a singleton substitution {X -> t} is [X]
prop_6 :: VarName -> Term -> Property
prop_6 x t = t /= Var x ==> domain (single x t) == [x]

-- The domain of a composed substitution is the union of the domains of the two substitutions
prop_7 :: Subst -> Subst -> Bool
prop_7 s1 s2 = all (`elem` (domain s1 ++ domain s2)) (domain (compose s1 s2))

-- The domain of a composed substitution does not contain variables that are mapped to themselves
prop_8 :: VarName -> VarName -> Property
prop_8 x1 x2 =
  x1
    /= x2
    ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2)))
    == [x2]

-- VarName "A"
-- VarName "_0"

-- The empty substitution does not contain any variables
prop_9 :: Bool
prop_9 = null (allVars empty)

-- The singleton substitution should not map a variable to itself
prop_10 :: VarName -> Bool
prop_10 x = null (allVars (single x (Var x)))

-- The variables occuring in a subsitution should be taken from both components of the individual substitutions
prop_11 :: VarName -> Term -> Property
prop_11 x t =
  t
    /= Var x
    ==> sort (nub (allVars (single x t)))
    == sort (nub (x : allVars t))

-- The variables occuring in a composed substitution are a subset of the variables occuring in the two substitutions
prop_12 :: Subst -> Subst -> Bool
prop_12 s1 s2 =
  all (`elem` (allVars s1 ++ allVars s2)) (allVars (compose s1 s2))

-- The composed subsitution should contain the left substitution unless its variables are mapped by the right substitution
prop_13 :: VarName -> VarName -> Property
prop_13 x1 x2 =
  x1
    /= x2
    ==> sort (allVars (compose (single x2 (Var x1)) (single x1 (Var x2))))
    == sort [x1, x2]

-- The domain of a substitution is a subset of all its variables
prop_14 :: Subst -> Bool
prop_14 s = all (`elem` allVars s) (domain s)

-- Restricting the empty substitution to an arbitrary set of variables should return the empty substitution
prop_15 :: [VarName] -> Bool
prop_15 xs = null (domain (restrictTo empty xs))

-- The domain of a restricted substitution is a subset of the given set of variables
prop_16 :: [VarName] -> Subst -> Bool
prop_16 xs s = all (`elem` xs) (domain (restrictTo s xs))



--um zyklische imports zu vermeiden haben wir uns entschieden, alle instanzen auf den Typen Subst auch in diese Klasse zu schreiben

--dieser Code Block braucht keine Kommentare oder? Sind immerhin doch recht einfache Funktionen..
domain :: Subst -> [VarName]
domain (Subst []) = []
domain (Subst l) = map fst l

restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst []) vl = Subst []
restrictTo (Subst list) vl = Subst (sus list vl)
  where
    sus :: [(VarName, Term)] -> [VarName] -> [(VarName, Term)]
    sus [] vl1 = []
    sus ((v1,t1):xs) vl1 = if elem v1 vl1 then [(v1,t1)] ++ sus xs vl1 else sus xs vl1 


empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single v (Var t) = if v /= t then Subst [(v,Var t)] else Subst []
single v t = Subst [(v,t)]

isEmpty :: Subst -> Bool
isEmpty s = if domain s == [] then True else False


-- applySingle :: Subst -> Term -> Term
-- applySingle(Subst []) (Var a) = Var a
-- applySingle (Subst [(v,t)]) (Var a) = if v == a then t else Var a
-- applySingle sub (Comb s []) = Comb s []
-- applySingle sub (Comb s (x:xs)) = Comb s ([applySingle sub x] ++ [applySingle sub (Comb s xs)])
-- applySingle _ _ = Var (VarName "")


-- apply :: Subst -> Term -> Term
-- apply (Subst []) (Var x) = Var x
-- apply (Subst []) (Comb s t) = Comb s t
-- apply (Subst [(v,t)]) term = applySingle (Subst [(v,t)]) term
-- apply (Subst ((v,t):xs)) term = apply (Subst xs) (applySingle (Subst [(v,t)]) term)


-- terms :: Subst -> [Term]
-- terms (Subst []) = []
-- terms (Subst s) = map snd s

-- subPlus :: Subst -> Subst -> Subst
-- subPlus (Subst x) (Subst y) = Subst (x ++ y)



-- composeSingle :: [(VarName, Term)] -> (VarName, Term) -> [(VarName, Term)]
-- composeSingle [] _ = []
-- composeSingle [(v1,Var t1)] (v2, Var t2) =  if t1 == v2 && v1 /= t2 then [(v1, Var t2)] else []
-- composeSingle [(v1,Var t1)] (v2, t2) = if t1 == v2 then [(v1, t2)] else []
-- composeSingle [(v1, t1)] (v2, t2) = []
-- composeSingle ((v1,Var t1):xs) (v2, Var t2) = if t1 == v2 && v1 /= t2 then [(v1, Var t2)] ++ composeSingle xs (v2,Var t2) else composeSingle xs (v2,Var t2)
-- composeSingle ((v1, Var t1):xs) (v2, t2) = if t1 == v2 then [(v1, t2)] ++ composeSingle xs (v2,t2) else composeSingle xs (v2,t2)
-- composeSingle ((v1, t1):xs) (v2, t2) = [(v1,t1)] ++ composeSingle xs (v2, t2)


-- domAdd :: Subst -> Subst -> Subst -> Subst 
-- domAdd s_fin (Subst []) c1 = subPlus s_fin c1
-- domAdd s_fin (Subst [(v,t)]) c1 = if notElem v (domain c1) then subPlus s_fin (Subst [(v,t)]) else s_fin
-- domAdd s_fin (Subst ((v,t):xs)) c1 = if notElem v (domain c1) then domAdd (subPlus s_fin (Subst [(v,t)])) (Subst xs) c1 else domAdd s_fin (Subst xs) c1


-- compose :: Subst -> Subst -> Subst
-- compose s1 s2 = compose_h (Subst []) s1 s2 s2
--   where
--     compose_h :: Subst -> Subst -> Subst -> Subst -> Subst
--     compose_h akku (Subst []) sub2 copys2 = subPlus akku sub2
--     compose_h akku sub1 (Subst []) copys2 = subPlus akku sub1
--     compose_h akku (Subst sub1) (Subst [(v2,t2)]) copys2 = domAdd (subPlus akku (Subst (composeSingle sub1 (v2,t2)))) copys2 (Subst sub1)
    -- compose_h akku (Subst sub1) (Subst ((v2,t2):xs2)) copys2 = compose_h  (subPlus akku (Subst (composeSingle sub1 (v2,t2)))) (Subst sub1) (Subst xs2) copys2


                          -- sub1           v2          t2
-- domain (compose (Subst "8" (Var "A")) (Subst "A" (Var "8")))


{-
alle Elemente aus sub1 werden nochmal mit sub2 substituiert
dabei darf sub2 die Elemente nicht zu denen machen, die sie in der (gleich i-ten) Substiution vor dem sub1 waren und
wir vereinigen mit allen Substitutionen aus sub2 die nicht in domain(sub1) sind?
-}

--auch Apply nochmal neu geschrieben. map bietet sich immer an, wenn man mit Datenstrukturen mit Listen arbeitet
apply :: Subst -> Term -> Term
apply (Subst []) term                   = term
apply (Subst (x:xs)) (Var z)            = if fst x == z then snd x else apply (Subst xs) (Var z)
apply (Subst (x:xs)) (Comb name [])     = Comb name []
apply (Subst (x:xs)) (Comb name list) = Comb name (map (apply (Subst (x:xs))) list)




isVar :: Term -> Bool --prüft ob ein Term eine einzelne Variable ist
isVar (Var _) = True
isVar _       = False



--         theta    sigma    komp
compose :: Subst -> Subst -> Subst -- wir haben erst versucht, das compose über verschachtelte Funktionsaufrufe zu implementieren, das hat aber irgendwie nicht so richtig geklappt (siehe auskommentieren Code oben)
compose (Subst []) (Subst [])         = Subst [] --deswegen haben wir dann einfach die Mengenschreibweise als Listcomprehension übersetzt, was sich 1 zu 1 übersetzen lässt
compose (Subst list) (Subst [])       = Subst list
compose (Subst []) (Subst list)       = Subst list
compose (Subst (x:xs)) (Subst (y:ys)) = Subst ([(k, apply (Subst (x:xs)) l) | (k,l) <- y:ys, not (isVar (apply (Subst (x:xs)) (l))) || (isVar (apply (Subst (x:xs)) (l)) && (apply (Subst (x:xs)) (l)) /= Var k), elem (fst y) [fst p | p <- y:ys], elem (snd y) [snd o | o <- y:ys]] 
                                                  ++ [(n,m) | (n,m) <- x:xs, notElem n (domain (Subst (y:ys)))])


intercalate' :: [a] -> [[a]] -> [a] --from the exercisegroup
intercalate' sep [] = []
intercalate' sep [x] = x
intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

-- instance Pretty Subst where
--   pretty (Subst []) = ""
--   pretty (Subst ((v,t):xs)) = "{" ++ intercalate' ", " [genSubst "" (Subst ((v,t):xs))] ++ "}"
--    where
--     genSubst akku (Subst []) = ""
--     genSubst akku (Subst ((v1,t1):xs1)) = genSubst (akku ++ show v1 ++ " -> " ++ show t1 ++ pretty (Subst xs1)) (Subst xs1)


instance Vars Subst where --generates all Variables from a Subst
  allVars (Subst []) = []
  allVars (Subst ((v,t):xs)) = [v] ++ allVars t ++ allVars (Subst xs)


instance Pretty Subst where --Pretty instance, we use intercalate for the commas and make it easy with putting the { and } just once, generating the rest in the function rrr
  pretty (Subst []) = "{}"
  pretty (Subst (x:xs)) = "{" ++ intercalate' ", " (rrr [] (x:xs)) ++ "}"
          where 
            rrr :: [String] -> [(VarName,Term)] -> [String]
            rrr akku []               = akku
            rrr akku ((m, Var n):ms)  = if Var m == Var n then rrr akku ms else rrr (akku ++ [pretty m ++ " -> " ++ pretty n]) ms
            rrr akku ((m, n):ms)      = rrr (akku ++ [pretty m ++ " -> " ++ pretty n]) ms



-- prop_8 :: VarName -> VarName -> Property
-- prop_8 x1 x2 =
  -- x1
    -- /= x2
    -- ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2)))
    -- == [x2]
    -- prop_8 "A" "8"
    


-- (restrictTo s (allVars t))
-- Run all tests
return []
testSubst :: IO Bool
testSubst = $quickCheckAll