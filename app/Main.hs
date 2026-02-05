{-|
Module      : Main
Description : Demonstration of ListUtils library functions
Copyright   : (c) Roo, 2024
License     : MIT

This executable demonstrates the usage of all functions
provided by the ListUtils module.
-}
module Main where

import ListUtils

main :: IO ()
main = do
    putStrLn "=== ListUtils Library Demo ==="
    putStrLn ""
    
    -- Demonstrate safeHead
    putStrLn "-- safeHead --"
    putStrLn $ "safeHead [1, 2, 3]    = " ++ show (safeHead [1, 2, 3 :: Int])
    putStrLn $ "safeHead []           = " ++ show (safeHead ([] :: [Int]))
    putStrLn $ "safeHead \"hello\"      = " ++ show (safeHead "hello")
    putStrLn ""
    
    -- Demonstrate safeLast
    putStrLn "-- safeLast --"
    putStrLn $ "safeLast [1, 2, 3]    = " ++ show (safeLast [1, 2, 3 :: Int])
    putStrLn $ "safeLast []           = " ++ show (safeLast ([] :: [Int]))
    putStrLn $ "safeLast \"hello\"      = " ++ show (safeLast "hello")
    putStrLn ""
    
    -- Demonstrate filterBy
    putStrLn "-- filterBy --"
    putStrLn $ "filterBy even [1..10] = " ++ show (filterBy even [1..10 :: Int])
    putStrLn $ "filterBy odd [1..10]  = " ++ show (filterBy odd [1..10 :: Int])
    putStrLn $ "filterBy (>5) [1..10] = " ++ show (filterBy (>5) [1..10 :: Int])
    putStrLn ""
    
    -- Demonstrate sumList
    putStrLn "-- sumList --"
    putStrLn $ "sumList [1..10]       = " ++ show (sumList [1..10 :: Int])
    putStrLn $ "sumList []            = " ++ show (sumList ([] :: [Int]))
    putStrLn $ "sumList [-5..5]       = " ++ show (sumList [-5..5 :: Int])
    putStrLn ""
    
    -- Demonstrate reverseList
    putStrLn "-- reverseList --"
    putStrLn $ "reverseList [1, 2, 3] = " ++ show (reverseList [1, 2, 3 :: Int])
    putStrLn $ "reverseList []        = " ++ show (reverseList ([] :: [Int]))
    putStrLn $ "reverseList \"hello\"   = " ++ show (reverseList "hello")
    putStrLn ""
    
    -- Demonstrate mapDouble
    putStrLn "-- mapDouble --"
    putStrLn $ "mapDouble [1, 2, 3]   = " ++ show (mapDouble [1, 2, 3 :: Int])
    putStrLn $ "mapDouble []          = " ++ show (mapDouble ([] :: [Int]))
    putStrLn $ "mapDouble [-1, 0, 1]  = " ++ show (mapDouble [-1, 0, 1 :: Int])
    putStrLn ""
    
    putStrLn "=== Demo Complete ==="
