module List
  ( head
  , length
  , replicate
  , take
  , foldl
  , foldr
  , filter
  , fmap
  , reverse
  , get
  , (<>)
  ) where

import Prelude hiding (head, length, replicate, take, foldl, foldr, filter, map, fmap, reverse, (<>), (++))

-- | Return the head of a list, if any
head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

-- | Return the length of a list
-- e.g. length [a, b, c] = 3
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

-- | Replicate the element n number of times in a list
-- e.g. replicate 4 'a' = ['a', 'a', 'a', 'a']
replicate :: Int -> a -> [a]
replicate 0 _ = [] 
replicate n x = x : replicate (n - 1) x 

-- | Take n xs, should return the prefix of xs of length n.
-- If length xs <= n, return xs
-- e.g. take 2 [a, b, c, d] = [a, b]
--      take 2 [a]          = [a]
take :: Int -> [a] -> [a]
take n xs
    | n <= 0 = []     
    | null xs = []      
take n (x:xs) = x : take (n - 1) xs

-- | Fold a list left-ways
-- e.g. foldl (+) 0 [a, b, c, d] = (((0 + a) + b) + c) + d
foldl :: (b -> a -> b) -> b -> [a] -> b 
foldl f y [] = y       
foldl f y (x:xs) = let y' = y `f` x in foldl f y' xs

-- | Fold a list right-ways
-- e.g. foldr (+) 0 [a, b, c, d] = a + (b + (c + (d + 0)))
--
-- Hint: It can be nice to write this using ($), which allows you to 
-- reduce parentheses. Try searching for it on hoogle!
-- e.g. f (g x) == f $ g x
-- Do try it out! It is a very common ideom to use ($) in Haskell.
foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f y [] = y
foldr f y (x:xs) = x `f` foldr f y xs

-- | Return a list containing only the elements that satisfy the predicate
-- e.g. filter odd [1, 2, 3, 4] = [1, 3]
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
--isEven n = n `mod` 2 == 0
--evenListNumbers xs = filter isEven xs
--isOdd n = n `mod` 2 == 1
--oddListNumbers xs = filter isEven xs
filter predicate xs = [ x | x <- xs, predicate x]

-- | Apply the passed function to all elements of the list
-- e.g. fmap (+3) [0, 5, -3] = [3, 8, 0]
fmap :: (a -> b) -> [a] -> [b]
fmap f [] = []
fmap f (x:xs) = f x : fmap f xs

-- | Reverse the passed list
-- e.g. reverse [a, b, c] = [c, b, a]
--
-- If you'd like a small challenge, you can actually try to implement this
-- without binding the input argument. I.e. by starting the function with 
-- reverse = ...
-- Try using one of your previously implemented functions for this :)
reverse :: [a] -> [a]
--reverse = foldl (\acc x -> x : acc) []
reverse = foldl (flip (:)) []

-- | Get the element at the given index
-- e.g. get 1  [a, b, c, d] = Just b
--      get 10 [a, b, c, d] = Nothing
get :: Int -> [a] -> Maybe a
get f [] = Nothing
get n (x:xs) 
    | n == 0 = Just (x)           
    
get n xs           
    | n < length xs = Just (xs !! n) 
    | otherwise = Nothing 



-- | Append two lists to each other
-- e.g. [3, 4] <> [1, 5] = [3, 4, 1, 5]
--
-- Hint: You may use both prefix and infix form 
--       pattern matching:
-- (<>) _ _ = ...
-- _ <> _ =  ...
(<>) :: [a] -> [a] -> [a]
--(<>) = (++)
--checking empty lists
[] <> ys = ys 
xs <> [] = xs
--xs <> (y:ys) = y : (xs <> ys)
(x:xs) <> (ys) = x : (xs <> ys)

