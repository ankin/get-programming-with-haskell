module Main where

import Control.Monad
import System.Environment
import Lesson22
import System.IO
import AoC

main :: IO ()
main = do
--  args <- getArgs
--  let linesToRead =
--        if length args > 0
--          then read (head args)
--          else 0 :: Int
--  numbers <- myReplicateM linesToRead getLine
--  let ints = map read numbers :: [Int]
--
--  print (sum ints)


--    userInput <- getContents
--    let reversed = reverse userInput
--    print reversed


-- # Advent of Code
  -- processFile "/Users/akinash/tmp/aoc/2.txt"
  -- validateFile "/Users/akinash/tmp/aoc/2.txt"

  calculateTreesInFile "/Users/akinash/tmp/aoc/3.txt"


-- mapM_ putStrLn args

-- Lesson 19
--  print (countOrgan Brain availableOrgans)
--  print (filter isSomething availableOrgans)
--
--  -- print (report (process Brain))
--
--  print (processRequest 14 organCatalog)
--
--  print (emptyDrawers availableOrgans)

-- Lesson 18

--  let organs = [Organ { key = 7, name = Heart }, Organ {key = 14, name = Spleen }]
--
--  let organCatalog = Map.fromList (map (\(Organ key name) -> (name, key)) organs)
--
--  print (organCatalog)
--  print (Map.lookup Heart organCatalog)
--  print (Map.lookup Kidney organCatalog)
--
--  let list = Cons 1 (Cons 2 (Cons 3 Empty))
--  print (ourMap (* 2) list)
--
--  print (transform (+ 1) aPoint)
--
--  let n = 9
--  print (Box n)
--
--  let word = "box"
--  print (Box word)

-- Lesson 17
--  print (mconcat [Green, Yellow, Blue])
--  print (mconcat [Green, Yellow, Blue, Transparent])
--
--  let coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])
--  let spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])
--  print (coin <> spinner)
--  print (mconcat [coin, coin, coin])
--
--  print (createPTable (Events ["tail", "heads"]) (Probs [0.5, 0.5]))
--  print (Green <> (Yellow <> Blue))

--  print (myAny even [1..5])

-- Capstone cipher
--  print (rotN 4 L4)
--  print (fourLetterAlphabetEncoder [L2, L3, L4, L1])
--  print (fourLetterAlphabetEncoder [L4, L1, L2, L3])
--  print (fourLetterAlphabetEncoder [L2, L3, L4, L1] == [L4, L1, L2, L3])
--
--  print (rotEncoder "Jean-Paul likes Simone")
--  print (rotDecoder "\557130\557157\557153\557166\557101\557136\557153\557173\557164\557088\557164\557161\557163\557157\557171\557088\557139\557161\557165\557167\557166\557157")
--
--  print (maxBits)

--  print (intToBits' 2)
--  print (intToBits' 8)
--
--  let bits = intToBits 16
--  print (bits)
--  print (bitsToInt bits)
--
--  print (applyOTP "Haskell" "Shhhhhh")
--
--  print (take 5 (concatMap intToBits (createCycle examplePrng 12345)))

-- Types / SixSidedDie
--    print ((S1 == S1))

-- Types / Patients
--  let janeESmith = Patient {name = MiddleName "Jane" "Elizabeth" "Smith", age = 30, sex = Female, height = 165, bloodType = BloodType O Neg}
--  let winstonSmith = Patient {name = Name "Winston" "Smith", age = 30, sex = Male, height = 174, bloodType = BloodType AB Pos}
--  print (patientCanDonateTo janeESmith winstonSmith)
--  print (patientCanDonateTo winstonSmith janeESmith)

--  Capstone  Robots
--  let strongRobot = robot ("strong", 12, 100)
--  let fatRobot = robot ("fat", 9, 130)
--
--  print "Before fight:"
--
--  print (printStats strongRobot)
--  print (printStats fatRobot)
--
--  let fatRobotRound1 = threeRoundFight strongRobot fatRobot
--
--  print "After 3-round fight:"
--  print (map printStats [fatRobotRound1])

-- Coffee cups
--  print (getOz coffeeCup)
--
--  let sip3 = drink coffeeCup 3
--
--  print ("sip 3")
--  print (getOz sip3)
--
--  let emptyCup = drink sip3 10
--
--  print ("sip 10")
--  print(getOz emptyCup)
--
--  print ("is empty")
--  print(isEmpty emptyCup)

-- misc

--  print (2 `elem2` [1 .. 5])
--
--  print (reverse3 [1..5])
--

--  print (reverse2 [1,2,3])
--
--  print (collatz 23)
--
--  print (take2 2 [1 .. 10])
--
--  print (length2 [])
--
--  print (myGCG2 14 12)
--
--  let myHostBuilder = getHostRequestBuilder "myHost"
--  let myResourceBuilder = getResourceRequestBuilder myHostBuilder "myApiKey" "myResource"
--  print (myResourceBuilder "123")
--  let myIdResource = getRequestUrl "blahhost" "123" "res1"
--  print (myIdResource "id1")
--  print (myIdResource "id2")
--
--  let substr2 = flip (-)
--  print (substr2 3 2)

-- let inc x = x +1
-- let ifEvenInc = ifEven inc
-- print (ifEvenInc 4)

-- print ([1..10] !! 11)

-- let repeat2 n = cycle[n]

-- print (take 4 (repeat2 4))

-- let subseq atStart atEnd aList = drop atStart (take atEnd aList)
-- print (subseq 2 7 "a puppy")

-- print (length [1..10] `div` 2)

--  let inFirstHalf e aList = elem e (take ((length aList) `div` 2) aList)
--  print (inFirstHalf 2 [1 .. 10])
