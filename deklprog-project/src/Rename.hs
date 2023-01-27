{-# LANGUAGE TemplateHaskell #-}

module Rename
  ( testRename
  , rename,
  )
where

import Data.List (intersect, nub)

import Test.QuickCheck

import Base.Type

import Vars
import Subst

-- append([E|R], L, [E|RL]) :- append(R, L, RL).

rename :: [VarName] -> Rule -> Rule
rename forbidden (Rule t ts) = Rule (apply subst t) (map (apply subst) ts)
  where vs = allVars (Rule t ts) -- E, R, L, RL
        ws = [ x | x <- freshVars, x `notElem` forbidden, x `notElem` vs ]
        pairs = zip vs ws -- [(E, A), (R, B), ...
        subst = foldl (\s (v,w) -> compose s (single v (Var w))) empty pairs
        -- ws = take (length vs) (filter (\x -> x `notElem` (forbidden ++ vs)) freshVars)

-- Properties


-- All variables in the renamed rule are fresh
prop_1 :: [VarName] -> Rule -> Bool
prop_1 xs r = null (allVars (rename xs r) `intersect` allVars r)

-- All variables in the renamed rule are not in the blocklist
prop_2 :: [VarName] -> Rule -> Bool
prop_2 xs r = null (allVars (rename xs r) `intersect` xs)

-- The number of variable names in the renamed rule equals number of variable names in the original rule
prop_3 :: [VarName] -> Rule -> Bool
prop_3 xs r = length (nub (allVars (rename xs r))) == length (nub (allVars r))

return []

testRename :: IO Bool
testRename = $(quickCheckAll)