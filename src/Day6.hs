module Day6
    ( day6
    ) where

import Paths_aoc_v2020
import Text.Read (readMaybe)
import Data.List.Split
import qualified Data.Foldable as Foldable
import Data.Set (Set)
import qualified Data.Set as Set

splitGroups :: String -> [String]
splitGroups = splitOn "\n\n"

splitPeople :: String -> [String]
splitPeople = lines

getUniqueAnswersPerGroup :: Set Char -> String -> Int
getUniqueAnswersPerGroup acc ('\n':rest) = getUniqueAnswersPerGroup acc rest 
getUniqueAnswersPerGroup acc (x:rest) = getUniqueAnswersPerGroup (Set.insert x acc) rest
getUniqueAnswersPerGroup acc [] = Set.size acc

answersToSets :: [String] -> [Set Char]
answersToSets = map Set.fromList

-- This is based on how Set.unions is implemented
intersections :: (Foldable f, Ord a) => f (Set a) -> Set a
intersections xs = Foldable.foldl' Set.intersection (Set.unions xs) xs

day6 :: IO()
day6 = do
    filePath <- getDataFileName "data/day6.txt"
    answersFile <- readFile filePath
    let groups = splitGroups answersFile
    let uniqueAnswers = map (getUniqueAnswersPerGroup Set.empty) groups
    putStrLn "Day 6"
    putStr "Part 1: "
    print (sum uniqueAnswers)
    let people = map splitPeople groups
    let answerSets = map answersToSets people
    let allAnswered = map intersections answerSets
    let numAllAnswered = map Set.size allAnswered
    putStr "Part 2: "
    print (sum numAllAnswered)
    putStrLn ""
