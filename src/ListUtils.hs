{-|
Module      : ListUtils
Description : Safe list operations and common list manipulation functions
Copyright   : (c) Roo, 2024
License     : MIT
Maintainer  : roo@example.com
Stability   : experimental

This module provides safe versions of partial list functions and
demonstrates common functional programming patterns including
filter, map, and fold operations.
-}
module ListUtils
  ( -- * Safe accessors
    safeHead
  , safeLast
    -- * Filtering
  , filterBy
    -- * Folding
  , sumList
  , reverseList
    -- * Mapping
  , mapDouble
  ) where

-- | Safe version of 'head' that returns 'Nothing' for empty lists.
--
-- ==== Examples
--
-- >>> safeHead [1, 2, 3]
-- Just 1
--
-- >>> safeHead []
-- Nothing
--
-- >>> safeHead "hello"
-- Just 'h'
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Safe version of 'last' that returns 'Nothing' for empty lists.
--
-- ==== Examples
--
-- >>> safeLast [1, 2, 3]
-- Just 3
--
-- >>> safeLast []
-- Nothing
--
-- >>> safeLast "hello"
-- Just 'o'
safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

-- | Filter a list using a predicate function.
-- This is a custom implementation demonstrating the filter pattern.
--
-- ==== Examples
--
-- >>> filterBy even [1, 2, 3, 4, 5, 6]
-- [2,4,6]
--
-- >>> filterBy (> 0) [-2, -1, 0, 1, 2]
-- [1,2]
--
-- >>> filterBy (const True) [1, 2, 3]
-- [1,2,3]
filterBy :: (a -> Bool) -> [a] -> [a]
filterBy _ []     = []
filterBy p (x:xs)
  | p x       = x : filterBy p xs
  | otherwise = filterBy p xs

-- | Sum all elements in a list using a fold.
-- Returns 0 for an empty list.
--
-- ==== Examples
--
-- >>> sumList [1, 2, 3, 4, 5]
-- 15
--
-- >>> sumList []
-- 0
--
-- >>> sumList [-1, 1]
-- 0
sumList :: Num a => [a] -> a
sumList = foldl (+) 0

-- | Reverse a list using a fold.
-- This demonstrates the fold pattern for list reversal.
--
-- ==== Examples
--
-- >>> reverseList [1, 2, 3]
-- [3,2,1]
--
-- >>> reverseList []
-- []
--
-- >>> reverseList "hello"
-- "olleh"
reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

-- | Double each element in a numeric list.
-- This demonstrates the map pattern.
--
-- ==== Examples
--
-- >>> mapDouble [1, 2, 3]
-- [2,4,6]
--
-- >>> mapDouble []
-- []
--
-- >>> mapDouble [-1, 0, 1]
-- [-2,0,2]
mapDouble :: Num a => [a] -> [a]
mapDouble = map (* 2)
