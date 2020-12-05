module AoC where

import Data.List.Split
import Text.Read

stringToNumbers :: String -> [Maybe Int]
stringToNumbers string = map readMaybe strArray
  where
    strArray = splitOn "\n" string

takeValues :: [Maybe Int] -> [Int]
takeValues [] = []
takeValues [Nothing] = []
takeValues [Just val] = [val]
takeValues (Nothing : xs) = takeValues xs
takeValues ((Just val) : xs) = val : takeValues xs

findPairs :: [Int] -> [(Int, Int)]
findPairs [] = []
findPairs list = pairs ++ findPairs xs
  where
    (x : xs) = list
    pairs = findPairsForElement x list
    findPairsForElement _ [] = []
    findPairsForElement element list = map (\el -> (element, el)) list

findTriples :: [Int] -> [(Int, Int)] -> [(Int, Int, Int)]
findTriples [] tuples = []
findTriples (x : xs) tuples = map (\(t1, t2) -> (t1, t2, x)) tuples ++ findTriples xs tuples

