module SLD
  ( SLDTree (..),
    sld,
    Strategy,
    dfs,
    bfs,
    solveWith,
  )
where

import Base.Type
import Subst
import Rename
import Unification
import Data.Maybe
import Vars
import Data.List hiding (findIndex)

-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving (Show)

sld :: Prog -> Goal -> SLDTree
sld (Prog []) g = SLDTree g []
sld p (Goal []) = SLDTree (Goal []) []
sld p g = sldHelp p g [] 

sldHelp :: Prog -> Goal -> [VarName] -> SLDTree
sldHelp (Prog []) g vars = SLDTree g []
sldHelp p (Goal []) vars = SLDTree (Goal []) []
sldHelp (Prog [Rule r rs]) (Goal [t]) vars = if isJust (unify r t) then SLDTree (Goal []) [(mkUni(unify r t), sldHelp (Prog [rename vars (Rule r rs)]) (Goal []) (vars ++ allVars (Rule r rs)))] else SLDTree (Goal [t]) []
sldHelp (Prog ((Rule r rs):rx)) (Goal [t]) vars = if isJust (unify r t)
                                                  then SLDTree (Goal []) [(mkUni(unify r t), sldHelp (Prog (renameList vars (Rule r rs:rx))) (Goal []) (vars ++ allVars (Rule r rs)))] --success
                                                  else SLDTree (Goal [t]) [] --clash
sldHelp (Prog [Rule r rs]) (Goal (t:ts)) vars =   if isJust (unify r t)
                                                  then SLDTree (Goal ts) [(mkUni(unify r t), sldHelp (Prog (renameList vars [Rule r rs])) (Goal ts) (vars ++ allVars (Rule r rs)))]
                                                  else SLDTree (Goal (t:ts)) [] -- clash
sldHelp (Prog ((Rule r rs):rx)) (Goal (t:ts)) vars =  if isJust (unify r t)
                                                        then SLDTree (Goal ts) [(mkUni(unify r t), sldHelp (Prog (renameList vars (Rule r rs:rx))) (Goal ts) (vars ++ allVars (Rule r rs)))] 
                                                        else sldHelp (Prog rx) (Goal (t:ts)) vars


renameList :: [VarName] -> [Rule] -> [Rule]
renameList vars r = map (rename vars) r 

mkUni :: Maybe Subst -> Subst
mkUni (Just s1) = s1
mkUni Nothing = empty


type Strategy = SLDTree -> [Subst]



-- pop :: Stack a -> (a, Stack a)
-- pop (Top a) = (a, St [])
-- pop (St [Top a, st]) = (a, st)

-- push :: Stack a -> a -> Stack a
-- push st x = St [Top x, st]

-- top :: Stack a -> a
-- top St [Top a, st] = a



dfs :: Strategy
dfs tree = dfsHelp tree [[0]] [tree]

-- treeAt :: [Int] -> SLDTree -> (Subst,SLDTree)
-- treeAt (x:[]) (SLDTree _ ts) = ts !! x
-- treeAt (x:xs) (SLDTree g t) = treeAt xs (SLDTree g [t !! x])
-- treeAt [] (SLDTree _ ts) = head ts

treeAtt :: [Int] -> SLDTree -> SLDTree
treeAtt [] t = t
treeAtt (x:[]) (SLDTree _ ts) = snd (ts !! x)
treeAtt (x:xs) (SLDTree g t) = treeAtt xs (SLDTree g [t !! x])


findSubTree :: SLDTree -> [[Int]] -> SLDTree
findSubTree t [[]] = t
findSubTree (SLDTree g []) visited = SLDTree g []
findSubTree (SLDTree g [t]) visited = if isInVisited (snd t) visited then (SLDTree g []) else snd t
findSubTree (SLDTree g (t:ts)) visited = if isInVisited (snd t) visited then findSubTree (SLDTree g ts) visited else snd t

isInVisited :: SLDTree -> [[Int]] -> Bool
isInVisited t [] = True
isInVisited t [v] = isTreeEq t (treeAtt v t)
isInVisited t (v:vs) = isTreeEq t (treeAtt v t) || isInVisited t vs


findIndex :: SLDTree -> [[Int]] -> Int -> [Int]
findIndex t [[]] i = []
findIndex (SLDTree g []) visited i = []
findIndex (SLDTree g (t:ts)) visited i = if isInVisited (snd t) visited then findIndex (SLDTree g ts) visited (i+1) else head (tail visited) ++ [i] 

hasValidSubTrees :: SLDTree -> [[Int]] -> Bool
hasValidSubTrees (SLDTree a []) [[]] = True
hasValidSubTrees (SLDTree a x) [[]] = False
hasValidSubTrees t visited = isTreeEq t (treeAtt (head visited) t) || hasValidSubTrees t (tail visited)

newTodo :: SLDTree -> [SLDTree] -> [[Int]] -> [SLDTree]
newTodo (SLDTree g []) todo visited = todo
newTodo t todo visited =  if hasValidSubTrees t visited 
                          then newTodo t (todo ++ [findSubTree t visited]) (visited ++ [findIndex t visited 0])
                          else todo


composeSubsts :: SLDTree -> [Int] -> Subst
composeSubsts (SLDTree g t) [] = empty
composeSubsts (SLDTree g t) path = compose (fst( t !! (head path))) (composeSubsts (snd(t !! (head path))) (tail path))


ismt :: Goal -> Bool
ismt (Goal []) = True
ismt _ = False

allTrees :: SLDTree -> [SLDTree]
allTrees (SLDTree g []) = []
allTrees (SLDTree g [t]) = [snd t]
allTrees (SLDTree g (t:ts)) = [snd t] ++ allTrees (SLDTree g ts)


isTListEq :: [SLDTree] -> [SLDTree] -> Bool
isTListEq [] [] = True
isTListEq [t] [t1] = isTreeEq t t1
isTListEq (t:ts) (t1:ts1) = isTreeEq t t1 && isTListEq ts ts1
isTListEq _ _ = False

isTreeEq :: SLDTree -> SLDTree -> Bool
isTreeEq (SLDTree g (t:ts)) (SLDTree g1 (t1:ts1)) = isGoalEq g g1 && fst t == fst t1 && isTListEq (allTrees (SLDTree g (t:ts))) (allTrees (SLDTree g1 (t1:ts1)))
isTreeEq _ _ = False

isGoalEq :: Goal -> Goal -> Bool
isGoalEq (Goal [t]) (Goal [t1]) = t == t1
isGoalEq _ _ = False

-- treeElem :: SLDTree -> [SLDTree] -> Bool
-- treeElem _ [] = False
-- treeElem t [t1] = isTreeEq t t1
-- treeElem t (t1:ts) = isTreeEq t t1 && treeElem t ts


-- isEl :: (Eq SLDTree) => SLDTree -> [SLDTree] -> Bool
-- isEl _ [] = False
-- isEl t [t1] = t == t1
-- isEl t (t1:ts) = t == t1 || isEl t ts

-- isEl (SLDTree g x) ((SLDTree g1 x1):f) = if g /= g1 then False else if (isEq x x1) then True else isEl (SLDTree g x) f


-- isEq :: (Eq Goal, Eq SLDTree) => [(Subst,SLDTree)] -> [(Subst,SLDTree)] -> Bool
-- isEq ((sub, SLDTree g x):t) ((sub1, SLDTree g1 x1):t1) = if g /= g1 || sub /= sub1 then False else isEq x x1


dfsHelp :: SLDTree -> [[Int]] -> [SLDTree] -> [Subst]
dfsHelp (SLDTree g ((sub1,t1):ts)) visited todo = if (hasValidSubTrees (SLDTree g ((sub1,t1):ts)) visited) 
                                                  then dfsHelp (findSubTree (SLDTree g ((sub1,t1):ts)) visited) (visited ++ [findIndex (SLDTree g ((sub1,t1):ts)) visited 0]) (newTodo (SLDTree g ((sub1,t1):ts)) todo (visited ++ [findIndex (SLDTree g ((sub1,t1):ts)) visited 0]))
                                                  else if ismt g 
                                                  then [composeSubsts (SLDTree g ((sub1,t1):ts)) (findIndex (SLDTree g ((sub1,t1):ts)) visited 0)] ++ dfsHelp (last todo) visited (reverse (tail (reverse todo)))
                                                  else dfsHelp (last todo) visited (reverse (tail (reverse todo)))
dfsHelp _ _ _ = []


bfs :: Strategy
bfs t = bfsHelp t [[0]] [t]

bfsHelp :: SLDTree -> [[Int]] -> [SLDTree] -> [Subst]
bfsHelp (SLDTree g ((sub1,t1):ts)) visited todo = if (hasValidSubTrees (SLDTree g ((sub1,t1):ts)) visited) 
                                                  then dfsHelp (findSubTree (SLDTree g ((sub1,t1):ts)) visited) (visited ++ [findIndex (SLDTree g ((sub1,t1):ts)) visited 0]) (newTodo (SLDTree g ((sub1,t1):ts)) todo (visited ++ [findIndex (SLDTree g ((sub1,t1):ts)) visited 0]))
                                                  else if ismt g 
                                                  then [composeSubsts (SLDTree g ((sub1,t1):ts)) (findIndex (SLDTree g ((sub1,t1):ts)) visited 0)] ++ dfsHelp (last todo) visited  (tail (reverse todo))
                                                  else dfsHelp (last todo) visited (reverse (tail (reverse todo)))
bfsHelp _ _ _ = []


solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g f = f (sld p g) 