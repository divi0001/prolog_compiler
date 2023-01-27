{-# LANGUAGE TemplateHaskell #-}

module Unification
  ( testUnification
  , unify
  ) where


import Data.Maybe

import Base.Type

import Vars

import Test.QuickCheck

import Subst

-- Properties

--  Uncomment this to test the properties when all required functions are implemented

-- The disagreement set of a term with itself is empty
prop_1 :: Term -> Bool
prop_1 t = isNothing (ds t t)

-- The disagreement set of two different terms is not empty
prop_2 :: Term -> Term -> Property
prop_2 t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

-- If a variable v occurs in a term t (other than the variable itself),
-- then the unification of v and t should fail due to the occur check
prop_3 :: VarName -> Term -> Property
prop_3 v t = occurs v t && t /= Var v ==> isNothing (unify (Var v) t)

-- If two terms t1 and t2 are unifiable, then the disagreement set of the mgu
-- applied to t1 and t2 is empty
prop_4 :: Term -> Term -> Property
prop_4 t1 t2 =
  let mMgu = unify t1 t2
  in isJust mMgu ==> let mgu = fromJust mMgu
                     in isNothing (ds (apply mgu t1) (apply mgu t2))


--Does  a variable occur in a term?
occurs :: VarName -> Term -> Bool
occurs v t = v `elem` allVars t




{-
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var x) (Var y) = if x /= y then Just (Var x, Var y) else Nothing
ds (Comb st1 t1) (Var x) = Just (Comb st1 t1, Var x)
ds (Var x) (Comb st1 t1) = Just (Var x, Comb st1 t1)
ds (Comb st1 []) t2 = if t2 /= Comb st1 [] then Just (Comb st1 [], t2) else Nothing
ds t1 (Comb st2 []) = if t1 /= Comb st2 [] then Just (t1, Comb st2 []) else Nothing
ds (Comb st1 (x:xs)) (Comb st2 (y:ys)) = if ds x y /= Nothing && x /= y then Just (x, y) else if x == y then Nothing else ds (Comb st1 xs) (Comb st2 ys)
-}

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var x) (Var y)                        = if x /= y then Just (Var x, Var y) else Nothing
ds (Var x) (Comb name term)               = Just (Var x, Comb name term)
ds (Comb name term) (Var x)               = Just (Var x, Comb name term)
ds (Comb name1 term1) (Comb name2 term2)  = if name1 == name2 && term1 == term2 then Nothing else Just (Comb name1 term1, Comb name2 term2)


unify :: Term -> Term -> Maybe Subst
unify t11 t21 = h1 t11 t21 (ds t11 t21) empty 
  where
    h1 :: Term -> Term -> Maybe (Term, Term) -> Subst -> Maybe Subst
    h1 (Var x) (Var y) (Just (t3, t4)) sub = if t3 /= t4 then Just (compose sub (single x (Var y))) else Nothing
    h1 (Comb st1 t1) (Var y) (Just (t3,t4)) sub = if y `elem` allVars (Comb st1 t1) then Nothing else Just (compose sub (single y (Comb st1 t1)))
    h1 (Var x) (Comb st2 t2) (Just (t3,t4)) sub = if x `elem` allVars (Comb st2 t2) then Nothing else Just (compose sub (single x (Comb st2 t2)))
    h1 (Comb st1 []) (Comb st2 t2) _ _ = Nothing
    h1 (Comb st1 t1) (Comb st2 []) _ _ = Nothing
    h1 (Comb st1 (t1:tx)) (Comb st2 (t2:ty)) (Just (t3, t4)) sub = if st1 /= st2 then Nothing else if (ds t1 t2 /= Nothing) then Just (compose sub (compose (notMaybe(h1 t1 t2 (ds t1 t2) sub))
                                                                   (notMaybe(h1 (Comb st1 tx) (Comb st2 ty) (ds (head tx) (head ty)) sub)) ) ) else Nothing
    h1 _ _ _ _ = Nothing
    notMaybe :: Maybe Subst -> Subst
    notMaybe (Just s1) = s1
    notMaybe Nothing = empty

-- Run all tests
return []
testUnification :: IO Bool
testUnification = $verboseCheckAll