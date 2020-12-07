module AoC where

import Data.List.Split
import System.IO
import Text.Read

-- #1

processFile :: String -> IO ()
processFile filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle

  let maybeNumbers = stringToNumbers contents
  let numbers = takeValues maybeNumbers
  -- #1.1
  let pairs = findPairs numbers
  --print pairs

  let filteredPairs = filter (\(x, y) -> x + y == 2020) pairs

  print "Matching pairs"
  print filteredPairs

  let multipliedTuples = map (\(x, y) -> x * y) filteredPairs
  print "Max product for tuples"
  print (maximum multipliedTuples)

  -- #1.2
  let triples = findTriples numbers pairs
  let filteredTriples = filter (\(x, y, z) -> x + y + z == 2020) triples
  print "Matching tripples"
  let multipliedTriples = map (\(x, y, z) -> x * y * z) filteredTriples
  print "Max product for triples"
  print (maximum multipliedTriples)

  hClose handle

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

-- #2

validateFile :: String -> IO ()
validateFile filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle

  print (length (splitOn "\n" contents))
  print (validate contents)

  hClose handle

validate :: String -> Int
validate contents = length (filter validateRule rows)
  where
    rows = map parseRow (rowsToParse)
    rowsToParse = filter (\s -> length s /= 0) (splitOn "\n" contents)

validateRule :: Row -> Bool
validateRule row = (minSize validationRule) <= textLength && textLength <= (maxSize validationRule)
  where
    matchChar = character (rule row)
    textToEvaluate = text row
    textLength = length (filter (== matchChar) textToEvaluate)
    validationRule = rule row

parseRow :: String -> Row
parseRow rowStr = Row {rule = parsedRule, text = parsedTextArr !! 0}
  where
    (ruleStr : parsedTextArr) = splitOn ": " rowStr
    parsedRule = parseRule ruleStr

parseRule :: String -> Rule
parseRule str = Rule {minSize = minB, maxSize = maxB, character = head (ch !! 0)}
  where
    (boundsStr : ch) = splitOn " " str
    boundsList = map (\s -> read s :: Int) (splitOn "-" boundsStr)
    (minB, maxB) = parseBonds boundsList
    parseBonds [] = error "invalid bounds"
    parseBonds [x] = error ("invalid bounds:" ++ show x ++ "in string " ++ str)
    parseBonds [mi, ma] = (mi, ma)

data Row = Row
  { rule :: Rule,
    text :: String
  }
  deriving (Show)

data Rule = Rule
  { minSize :: Int,
    maxSize :: Int,
    character :: Char
  }
  deriving (Show)

calculateTreesInFile :: String -> IO ()
calculateTreesInFile fileName = do
  fileHandle <- openFile fileName ReadMode
  contents <- hGetContents fileHandle

  print (calculateTrees contents)

  hClose fileHandle

calculateTrees :: String -> Int
calculateTrees content = length (treesOnly)
  where
    (_ : rows) = toRows content -- start with second row
    rowsWithIndices = zip rows [3, 6 ..] -- so indices start with 3
    bitmap = map isTree rowsWithIndices
    treesOnly = filter (== True) bitmap

toRows :: String -> [String]
toRows contents = map cycle rows
  where
    rows = filter (not . null) (splitOn "\n" contents)

isTree :: (String, Int) -> Bool
isTree (row, pos) = row !! pos == '#'
