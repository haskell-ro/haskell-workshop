module Main where

import Control.Monad.State
import Data.List

import Debug.Trace

data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

t1 = Node (Leaf 6) 5 (Leaf 1)
t2 = Node t1 4 (Leaf 3)
t3 = Node (Leaf 8) 7 (Leaf 9)
myTree = Node t2 2 t3

minTree (Leaf x) = x
minTree (Node l v r) = minimum [minTree l, v, minTree r]

getTreeRoot (Leaf x) = x
getTreeRoot (Node _ x _) = x

sumTree (Leaf x) = Leaf x
sumTree (Node l v r) = Node l' v' r'
  where
    l' = sumTree l
    r' = sumTree r
    v' = v + getTreeRoot l' + getTreeRoot r'

depthTree t = go t 0
  where
    go (Leaf _) d = Leaf d
    go (Node l _ r) d = Node (go l $ d + 1) d (go l $ d + 1)

zipTree (Leaf a) (Leaf b) = Leaf (a, b)
zipTree (Leaf a) (Node _ b _) = Leaf (a, b)
zipTree (Node _ a _) (Leaf b) = Leaf (a, b)
zipTree (Node l1 a r1) (Node l2 b r2) = Node (zipTree l1 l2) (a, b) (zipTree r1 r2)

complex t = zipTree (sumTree t) (depthTree t)

complex' t = go t 0
  where
    go (Leaf x) d = Leaf (x, d)
    go (Node l v r) d = Node l' (v', d) r'
      where
        l' = go l $ d + 1
        r' = go r $ d + 1
        v' = v + fst (getTreeRoot l') + fst (getTreeRoot r')

transform (Leaf x) = do
  depth <- get
  return $ Leaf (x, depth)
transform (Node l v r) = do
  l' <- transform l
  r' <- transform r
  depth <- get
  let root_l = fst $ getTreeRoot l'
  let root_r = fst $ getTreeRoot r'
  return $ Node l' (v + root_l + root_r, depth) r'

complex'' t = evalState (transform t) 0

find3 p1 l1 p2 l2 p3 l3 = do
  t1 <- find p1 l1 >>= liftM (\x -> trace ("p1 returned " ++ show x) x) . return
  t2 <- find p2 l2 >>= liftM (\x -> trace ("p2 returned " ++ show x) x) . return
  t3 <- find p3 l3 >>= liftM (\x -> trace ("p3 returned " ++ show x) x) . return
  return (t1, t2, t3)

find3' p1 l1 p2 l2 p3 l3 =
  case find p1 l1 of
    Just t1 -> case find p2 l2 of
      Just t2 -> case find p3 l3 of
        Just t3 -> Just (t1, t2, t3)
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

main = do
  x <- return 42
  s <- getLine
  print (x, s)
