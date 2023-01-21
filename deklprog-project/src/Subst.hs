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
import Test.QuickCheck
import Vars
import Pretty

-- Data type for substitutions
data Subst = Subst [(VarName, Term)]
  deriving (Show)

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


instance Vars Subst where
  allVars (Subst []) = []
  allVars (Subst ((v,t):xs)) = [v] ++ allVars t ++ allVars (Subst xs)



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
single v t = Subst [(v,t)]

isEmpty :: Subst -> Bool
isEmpty s = if domain s == [] then True else False


applySingle :: Subst -> Term -> Term
applySingle(Subst []) (Var a) = Var a
applySingle (Subst [(v,t)]) (Var a) = if v == a then t else Var a
applySingle sub (Comb s []) = Comb s []
applySingle sub (Comb s (x:xs)) = Comb s ([applySingle sub x] ++ [applySingle sub (Comb s xs)])
applySingle _ _ = Var (VarName "")


apply :: Subst -> Term -> Term
apply (Subst []) (Var x) = Var x
apply (Subst []) (Comb s t) = Comb s t
apply (Subst [(v,t)]) term = applySingle (Subst [(v,t)]) term
apply (Subst ((v,t):xs)) term = apply (Subst xs) (applySingle (Subst [(v,t)]) term)


-- terms :: Subst -> [Term]
-- terms (Subst []) = []
-- terms (Subst s) = map snd s

subPlus :: Subst -> Subst -> Subst
subPlus (Subst x) (Subst y) = Subst (x ++ y)

compose :: Subst -> Subst -> Subst
compose sub1 (Subst []) = sub1
compose (Subst []) sub2 = sub2
compose (Subst ((v,t):xs)) sub2 = if v `notElem` allVars(apply sub2 t) then subPlus (Subst [(v, apply sub2 t)]) (compose (Subst xs) sub2) else compose (Subst xs) sub2



intercalate' :: [a] -> [[a]] -> [a]
intercalate' sep [] = []
intercalate' sep [x] = x
intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

instance Pretty Subst where
  pretty (Subst []) = ""
  pretty (Subst ((v,t):xs)) = "{" ++ intercalate' ", " [genSubst "" (Subst ((v,t):xs))] ++ "}"
   where
    genSubst akku (Subst []) = ""
    genSubst akku (Subst ((v1,t1):xs1)) = genSubst (akku ++ show v1 ++ " -> " ++ show t1 ++ pretty (Subst xs1)) (Subst xs1)

{-
alle Elemente aus sub1 werden nochmal mit sub2 substituiert
dabei darf sub2 die Elemente nicht zu denen machen, die sie in der (gleich i-ten) Substiution vor dem sub1 waren und
wir vereinigen mit allen Substitutionen aus sub2 die nicht in domain(sub1) sind.
-}





-- (restrictTo s (allVars t))
-- Run all tests
return []
testSubst :: IO Bool
testSubst = $quickCheckAll