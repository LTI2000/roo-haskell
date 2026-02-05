{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : ListUtilsSpec
Description : Comprehensive tests for ListUtils module
Copyright   : (c) Roo, 2024
License     : MIT

This module contains both unit tests (HSpec) and property-based tests
(QuickCheck) for all functions in the ListUtils module.
-}
module ListUtilsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import ListUtils

spec :: Spec
spec = do
    describe "safeHead" $ do
        -- Unit tests
        it "returns Nothing for empty list" $
            safeHead ([] :: [Int]) `shouldBe` Nothing
        
        it "returns Just the element for single element list" $
            safeHead [42 :: Int] `shouldBe` Just 42
        
        it "returns Just the first element for multi-element list" $
            safeHead [1, 2, 3 :: Int] `shouldBe` Just 1
        
        it "works with String type" $
            safeHead "hello" `shouldBe` Just 'h'
        
        it "works with Bool type" $
            safeHead [True, False] `shouldBe` Just True
        
        -- Property tests
        it "prop: empty list always returns Nothing" $
            property $ \(_ :: Int) -> safeHead ([] :: [Int]) == Nothing
        
        it "prop: non-empty list returns Just (head xs)" $
            property $ \(NonEmpty xs) -> 
                safeHead (xs :: [Int]) == Just (head xs)
        
        it "prop: singleton list returns Just that element" $
            property $ \(x :: Int) -> 
                safeHead [x] == Just x

    describe "safeLast" $ do
        -- Unit tests
        it "returns Nothing for empty list" $
            safeLast ([] :: [Int]) `shouldBe` Nothing
        
        it "returns Just the element for single element list" $
            safeLast [42 :: Int] `shouldBe` Just 42
        
        it "returns Just the last element for multi-element list" $
            safeLast [1, 2, 3 :: Int] `shouldBe` Just 3
        
        it "works with String type" $
            safeLast "hello" `shouldBe` Just 'o'
        
        it "works with Bool type" $
            safeLast [True, False] `shouldBe` Just False
        
        -- Property tests
        it "prop: empty list always returns Nothing" $
            property $ \(_ :: Int) -> safeLast ([] :: [Int]) == Nothing
        
        it "prop: non-empty list returns Just (last xs)" $
            property $ \(NonEmpty xs) -> 
                safeLast (xs :: [Int]) == Just (last xs)
        
        it "prop: singleton list returns Just that element" $
            property $ \(x :: Int) -> 
                safeLast [x] == Just x
        
        it "prop: safeHead xs == safeLast (reverseList xs)" $
            property $ \(xs :: [Int]) -> 
                safeHead xs == safeLast (reverseList xs)

    describe "filterBy" $ do
        -- Unit tests
        it "filters even numbers correctly" $
            filterBy even [1, 2, 3, 4, 5, 6 :: Int] `shouldBe` [2, 4, 6]
        
        it "filters odd numbers correctly" $
            filterBy odd [1, 2, 3, 4, 5, 6 :: Int] `shouldBe` [1, 3, 5]
        
        it "filters positive numbers correctly" $
            filterBy (> 0) [-2, -1, 0, 1, 2 :: Int] `shouldBe` [1, 2]
        
        it "returns original list with always-true predicate" $
            filterBy (const True) [1, 2, 3 :: Int] `shouldBe` [1, 2, 3]
        
        it "returns empty list with always-false predicate" $
            filterBy (const False) [1, 2, 3 :: Int] `shouldBe` []
        
        it "returns empty list when filtering empty list" $
            filterBy even ([] :: [Int]) `shouldBe` []
        
        -- Property tests
        it "prop: filtered list length <= original length" $
            property $ \(xs :: [Int]) -> 
                length (filterBy even xs) <= length xs
        
        it "prop: filterBy (const True) returns original list" $
            property $ \(xs :: [Int]) -> 
                filterBy (const True) xs == xs
        
        it "prop: filterBy (const False) returns empty list" $
            property $ \(xs :: [Int]) -> 
                filterBy (const False) xs == []
        
        it "prop: all elements in result satisfy predicate" $
            property $ \(xs :: [Int]) -> 
                all even (filterBy even xs)

    describe "sumList" $ do
        -- Unit tests
        it "returns 0 for empty list" $
            sumList ([] :: [Int]) `shouldBe` 0
        
        it "returns the element for single element list" $
            sumList [42 :: Int] `shouldBe` 42
        
        it "sums positive numbers correctly" $
            sumList [1, 2, 3, 4, 5 :: Int] `shouldBe` 15
        
        it "handles negative numbers correctly" $
            sumList [-1, -2, 3 :: Int] `shouldBe` 0
        
        it "handles mixed positive and negative" $
            sumList [-5, 0, 5 :: Int] `shouldBe` 0
        
        -- Property tests
        it "prop: sum of empty list is 0" $
            property $ \(_ :: Int) -> sumList ([] :: [Int]) == 0
        
        it "prop: sum of singleton equals the element" $
            property $ \(x :: Int) -> 
                sumList [x] == x
        
        it "prop: sum of concatenation equals sum of sums" $
            property $ \(xs :: [Int]) (ys :: [Int]) -> 
                sumList (xs ++ ys) == sumList xs + sumList ys
        
        it "prop: sum is commutative (via reverse)" $
            property $ \(xs :: [Int]) -> 
                sumList xs == sumList (reverseList xs)

    describe "reverseList" $ do
        -- Unit tests
        it "returns empty list for empty input" $
            reverseList ([] :: [Int]) `shouldBe` []
        
        it "returns same list for single element" $
            reverseList [42 :: Int] `shouldBe` [42]
        
        it "reverses multi-element list correctly" $
            reverseList [1, 2, 3 :: Int] `shouldBe` [3, 2, 1]
        
        it "reverses string correctly" $
            reverseList "hello" `shouldBe` "olleh"
        
        -- Property tests
        it "prop: reversing twice gives original list" $
            property $ \(xs :: [Int]) -> 
                reverseList (reverseList xs) == xs
        
        it "prop: reverse preserves length" $
            property $ \(xs :: [Int]) -> 
                length (reverseList xs) == length xs
        
        it "prop: head of reversed equals last of original (non-empty)" $
            property $ \(NonEmpty xs) -> 
                safeHead (reverseList (xs :: [Int])) == safeLast xs
        
        it "prop: last of reversed equals head of original (non-empty)" $
            property $ \(NonEmpty xs) -> 
                safeLast (reverseList (xs :: [Int])) == safeHead xs

    describe "mapDouble" $ do
        -- Unit tests
        it "returns empty list for empty input" $
            mapDouble ([] :: [Int]) `shouldBe` []
        
        it "doubles single element" $
            mapDouble [5 :: Int] `shouldBe` [10]
        
        it "doubles all elements in list" $
            mapDouble [1, 2, 3 :: Int] `shouldBe` [2, 4, 6]
        
        it "handles negative numbers" $
            mapDouble [-1, 0, 1 :: Int] `shouldBe` [-2, 0, 2]
        
        it "handles zero" $
            mapDouble [0 :: Int] `shouldBe` [0]
        
        -- Property tests
        it "prop: mapDouble preserves length" $
            property $ \(xs :: [Int]) -> 
                length (mapDouble xs) == length xs
        
        it "prop: each element is doubled" $
            property $ \(xs :: [Int]) -> 
                mapDouble xs == map (* 2) xs
        
        it "prop: mapDouble of empty list is empty" $
            property $ \(_ :: Int) -> 
                mapDouble ([] :: [Int]) == []
        
        it "prop: mapDouble of singleton [x] equals [2*x]" $
            property $ \(x :: Int) -> 
                mapDouble [x] == [2 * x]
        
        it "prop: sum of doubled list equals 2 * sum of original" $
            property $ \(xs :: [Int]) -> 
                sumList (mapDouble xs) == 2 * sumList xs
