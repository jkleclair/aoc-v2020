module Day5
    ( day5
    ) where

import Paths_aoc_v2020
import Text.Read (readMaybe)
import Data.Bits
import Data.Sort

-- TODO: Use zip to keep track of 2^x instead of using index
transformSeatPiece :: Int -> Int -> String -> Int
transformSeatPiece finalBit _ [] = finalBit
transformSeatPiece curBit index (x:xs) = transformSeatPiece (curBit .|. or) (index + 1) xs
    where
        or  = (2 ^ index) * bit
        bit = case x of 'F' -> 0
                        'L' -> 0
                        'B' -> 1
                        'R' -> 1

parseSeat :: String -> (Int, Int)
parseSeat str = (rowId, columnId)
    where
        (row, column) = span isRowPiece str
        rowId = transformSeatPiece 0 0 (reverse row) 
        columnId = transformSeatPiece 0 0 (reverse column)

isRowPiece :: Char -> Bool
isRowPiece x = x == 'F' || x == 'B'

findSeat :: [Int] -> Int
findSeat (x:[]) = 0
findSeat (x:y:rest)
    | (x + 1) == y = findSeat (y:rest)
    | otherwise    = x + 1

seatId :: (Int, Int) -> Int
seatId (row, column) = row * 8 + column

day5 :: IO()
day5 = do
    filePath <- getDataFileName "data/day5.txt"
    seatsFile <- readFile filePath
    let seats = lines seatsFile
    let parsedSeats = map parseSeat seats
    let seatIds = map seatId parsedSeats
    putStrLn "Day 5"
    putStr "Part 1: "
    print (maximum seatIds)
    putStr "Part 2: "
    print (findSeat (sort seatIds))
    putStrLn ""
