module Tree
  ( Tree (..)
  , insert
  , contains
  , fmap
  , foldl
  , foldr
  ) where

import Prelude hiding (fmap, foldl, foldr)

data Tree a 
  = Node (Tree a) a (Tree a)
  | Leaf
  deriving (Show, Eq)

-- | Insert an element into the tree in an 
-- ordered fashion. Don't do anything on duplicates 
-- (leave the tree as it is). No points if not O(log n)
-- for the average case (so you don't have to balance the Tree)
--
-- e.g. insert 5
--   2          2
--  / \   ->   / \
-- 0   9      0   9
--               /
--              5
--
-- Hint: Instead of using (<), (>) and family,
--       Try using 'compare' and pattern match the result
insert :: Ord a => a -> Tree a -> Tree a
-- insert x Leaf = do
--  let newNode = Node Leaf x Leaf
--  return newNode
insert x Leaf = Node Leaf x Leaf -- new node in empty tree

insert x (Node left a right)
  | x < a  = Node (insert x left) a right -- inserting to left
  | x > a  = Node left a (insert x right)  -- inserting to right
  | otherwise = Node left a right  -- duplicates

-- | Returns True if the element is contained
-- in the tree, otherwise False. 
-- No points if not O(log n)
contains :: Ord a => a -> Tree a -> Bool
contains _ Leaf = False --empty

contains x (Node left a right)
  | x == a = True -- element is contained   
  | x < a  = contains x left -- check left
  | otherwise  = contains x right -- check right

-- | Apply the passed function to all elements of the tree
-- e.g. fmap (+2) (Node Leaf 4 (Node Leaf 5 Leaf)) = (Node Leaf 6 (Node Leaf 7 Leaf))
fmap :: (a -> b) -> Tree a -> Tree b
fmap = undefined

-- | Fold the tree from smallest to largest
-- e.g. foldl + 0 (Node Leaf 4 (Node Leaf 5 Leaf)) = (0 + 4) + 5
foldl :: (b -> a -> b) -> b -> Tree a -> b
foldl = undefined

-- | Fold the tree from largest to smallest
-- e.g. foldr + 0 (Node Leaf 4 (Node Leaf 5 Leaf)) = 4 + (5 + 0)
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr = undefined
