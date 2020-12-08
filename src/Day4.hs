module Day4
    ( day4
    ) where

import Paths_aoc_v2020
import Text.Read (readMaybe)
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Regex.Posix

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredFields :: [String] -> Map String String -> Bool
hasRequiredFields fields passport = and (map (`Map.member` passport) fields)

areFieldsValid :: Map String String -> Bool
areFieldsValid = Map.foldrWithKey (\key value acc -> acc && isFieldValid key value) True

isFieldValid :: String -> String -> Bool
isFieldValid key value
    | key == "byr" = value =~ "^(19[2-9][0-9]|200[0-2])$" :: Bool
    | key == "iyr" = value =~ "^(201[0-9]|2020)$" :: Bool
    | key == "eyr" = value =~ "^(202[0-9]|2030)$" :: Bool
    | key == "hgt" = value =~ "^(1[5-8][0-9]|19[0-3])cm$|^(59|6[0-9]|7[0-6])in$" :: Bool
    | key == "hcl" = value =~ "^#[0-9|a-f]{6}$" :: Bool
    | key == "ecl" = value =~ "^(amb|blu|brn|gry|grn|hzl|oth)$" :: Bool
    | key == "pid" = value =~ "^[0-9]{9}$" :: Bool
    | key == "cid" = True
    | otherwise    = False
 
splitPassports :: String -> [String]
splitPassports = splitOn "\n\n"        

splitPassportData :: String -> [String]
splitPassportData = splitOneOf " \n"

parsePassportFields :: Map String String -> [String] -> Map String String
parsePassportFields acc [] = acc
parsePassportFields acc (x:xs)
    | x == "" = parsePassportFields acc xs
    | otherwise = parsePassportFields newAcc xs
    where
        (key, value) = break (==':') x
        newAcc = Map.insert key (tail value) acc

day4 :: IO()
day4 = do
    filePath <- getDataFileName "data/day4.txt"
    passportsFile <- readFile filePath
    let passports = splitPassports passportsFile
    let passportsSplit = map splitPassportData passports
    let parsedPassports = map (parsePassportFields Map.empty) passportsSplit
    let requiredFieldsPassports = map (hasRequiredFields requiredFields) parsedPassports
    let validFieldsPassports = map areFieldsValid parsedPassports
    let requiredAndValidFieldsPassports = zipWith (&&) requiredFieldsPassports validFieldsPassports
    putStrLn ("Day 4")
    putStrLn ("Part 1: ")
    print (length (filter (==True) requiredFieldsPassports))
    putStrLn ("Part 2: ")
    print (length (filter (==True) requiredAndValidFieldsPassports))
