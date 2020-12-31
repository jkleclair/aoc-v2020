module Day3
    ( day3
    ) where

import Paths_aoc_v2020
import Text.Read (readMaybe)

type Trees = Int
type Right = Int
type Down = Int
type Index = Int
type Slope = (Right, Down)

testSlope = (3,1)
testSlopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

-- TODO: Can you do this with a map?
findTrees :: Trees -> Index -> [String] -> Slope -> Trees
findTrees acc _ [] _ = acc
findTrees acc index (x:xs) (right, down)
    | shouldCheck down index && (x !! curRight) == '#' = findTrees (acc + 1) (index + 1) xs (right, down)
    | otherwise                                        = findTrees acc (index + 1) xs (right, down)
    where
        curRight = right * (index `div` down)

shouldCheck :: Down -> Index -> Bool
shouldCheck d i = (i `mod` d) == 0

day3 :: IO()
day3 = do
    filePath <- getDataFileName "data/day3.txt"
    slopeFile <- readFile filePath
    let slopes = lines slopeFile
    let infiniteSlopes = map cycle slopes
    let trees = findTrees 0 0 infiniteSlopes testSlope
    -- TODO: Calculate all slopes in one run instead of indidually 
    let trees' = map (findTrees 0 0 infiniteSlopes) testSlopes
    putStrLn "Day 3"
    putStr "Part 1: "
    print (trees)
    putStr "Part 2: "
    print (product trees')
    putStrLn ""
    
