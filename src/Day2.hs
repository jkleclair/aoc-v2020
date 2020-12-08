module Day2
    ( day2
    ) where

import Paths_aoc_v2020
import Text.Read (readMaybe)
import Data.List.Split
import Control.Lens

data Password = Password {
    minRange :: Int,
    maxRange :: Int,
    testChar :: Char,
    password :: String
} deriving (Show)

-- this could fail on bad input
parsePassword :: String -> Password
parsePassword line = Password minRange maxRange testChar password
    where
        pieces = words line
        range = head' pieces
        (minRange, maxRange) = case parseRange range of (Nothing, Nothing) -> (0, 0)
                                                        (Just x, Nothing) -> (0, 0)
                                                        (Nothing, Just x) -> (0, 0)
                                                        (Just x, Just y) -> (x, y)
        testChar = case (pieces ^? ix 1) of Nothing -> ' '
                                            Just x  -> head x
        password = last pieces


parseRange :: Maybe String -> (Maybe Int, Maybe Int)
parseRange Nothing = (Nothing, Nothing)
parseRange (Just str) = (minRange, maxRange)
    where
        range = splitOn "-" str
        minRange = case (head' range) of Nothing -> Nothing
                                         Just x  -> readMaybe x
        maxRange = case (last' range) of Nothing -> Nothing
                                         Just x  -> readMaybe x

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

last' :: [a] -> Maybe a
last' [] = Nothing
last' (x:[]) = Just x
last' (x:xs) = last' xs

countChar :: Char -> String -> Int
countChar testChar = length . filter (== testChar)

passwordIsValid :: Password -> Bool
passwordIsValid (Password minRange maxRange testChar password) = minRange <= count && count <= maxRange
    where
        count = countChar testChar password

passwordIsValid' :: Password -> Bool
passwordIsValid' (Password minRange maxRange testChar password) = (first == testChar) `xor` (second == testChar)
    where
        first = case (password ^? ix (minRange - 1)) of Nothing -> ' '
                                                        Just x -> x
        second = case (password ^? ix (maxRange - 1)) of Nothing -> ' '
                                                         Just x -> x

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

day2 :: IO()
day2 = do
    filePath <- getDataFileName "data/day2.txt"
    passwordsFile <- readFile filePath
    let passwords = lines passwordsFile
    let parsedPasswords = map parsePassword passwords
    let validPasswords = map passwordIsValid parsedPasswords
    let numValid = length (filter (== True) validPasswords)
    let validPasswords' = map passwordIsValid' parsedPasswords
    let numValid' = length (filter (== True) validPasswords')
    putStrLn ("Day 2")
    putStrLn ("Part 1: ")
    print(numValid)
    putStrLn ("Part 2: ")
    print(numValid')
    putStrLn ("")

