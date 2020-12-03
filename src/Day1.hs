module Day1
    ( day1
    ) where

import Paths_aoc_v2020
import Text.Read (readMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

target = 2020

twoSum :: [Maybe Int] -> Set Int -> Int -> Maybe Int
twoSum ((Just cur):rest) vals target
    | Set.member complement vals = Just (cur * complement)
    | otherwise = twoSum rest (Set.insert cur vals) target
    where
        complement = target - cur
twoSum (Nothing:_) _ _ = Nothing
twoSum [] _ _ = Nothing

threeSum :: [Maybe Int] -> Int -> Maybe Int
threeSum ((Just cur):rest) target
    | Just val <- two = Just (cur * val)
    | otherwise = threeSum rest target
    where
        two = twoSum rest Set.empty complement
        complement = target - cur
threeSum (Nothing:_) _ = Nothing
threeSum [] _ = Nothing

day1 :: IO()
day1 = do
    filePath <- getDataFileName "data/day1.txt"
    numbersFile <- readFile filePath
    let numbersFileSplit = lines numbersFile
    let numbers = map readMaybe numbersFileSplit
    putStrLn ("Day 1")
    putStrLn ("Part 1:")
    print (twoSum numbers Set.empty target)
    putStrLn ("Day 1")
    putStrLn ("Part 2:")
    print (threeSum numbers target)
