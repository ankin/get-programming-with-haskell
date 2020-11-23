module Capstone20 where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup

file1 :: [(Int, Double)]
file1 =
  [ (1, 200.1),
    (2, 199.5),
    (3, 199.4),
    (4, 198.9),
    (5, 199.0),
    (6, 200.2),
    (9, 200.3),
    (10, 201.2),
    (12, 202.9)
  ]

file2 :: [(Int, Double)]
file2 =
  [ (11, 201.6),
    (12, 201.5),
    (13, 201.5),
    (14, 203.5),
    (15, 204.9),
    (16, 207.1),
    (18, 210.5),
    (20, 208.8)
  ]

file3 :: [(Int, Double)]
file3 =
  [ (10, 201.2),
    (11, 201.6),
    (12, 201.5),
    (13, 201.5),
    (14, 203.5),
    (17, 210.5),
    (24, 215.1),
    (25, 218.7)
  ]

file4 :: [(Int, Double)]
file4 =
  [ (26, 219.8),
    (27, 220.5),
    (28, 223.8),
    (29, 222.8),
    (30, 223.8),
    (31, 221.7),
    (32, 222.3),
    (33, 220.8),
    (34, 219.4),
    (35, 220.1),
    (36, 220.6)
  ]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where
    completeTimes = [minimum times .. maximum times] -- artificially creates time, which wasn't present in a file :/
    timeValueMap = Map.fromList (zip times values)
    extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS pairs = createTS times values
  where
    (times, values) = unzip pairs

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where
      rows = zipWith showTVPair times values

-- my impl
--compbineTS :: TS a -> TS a -> TS a
--compbineTS (TS keys1 vals1) (TS keys2 vals2) = createTS newKeys newValues
--  where ts1Map = Map.fromList (zip keys1 vals1)
--        ts2Map = Map.fromList (zip keys2 vals2)
--        unionMap = Map.union ts2Map ts1Map
--        newKeys = Map.keys unionMap
--        newValues = map (\k -> myflatten(Map.lookup k unionMap) newKeys
--
--
--myflatten :: (Maybe (Maybe a)) -> Maybe a
--myflatten Just (Just a) = Just a
--myflatten (Just Nothing) = Nothing
--myflatten Nothing = Nothing

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, (Just value)) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
    combinedValues = map (\k -> Map.lookup k updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mappend = (<>)
  mempty = TS [] []

mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = realToFrac (sum xs)
    count = (realToFrac . length) xs -- another way of composing functions

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS _ values) =
  if all (== Nothing) values
    then Nothing
    else Just avg
  where
    justVals = filter isJust values
    cleanVals = map fromJust justVals
    avg = mean cleanVals

type CompareFunc a = a -> a -> a

type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where
    newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
    newFunc (_, Nothing) (i, val) = (i, val)
    newFunc (i, val) (_, Nothing) = (i, val)
    newFunc (i1, Just val1) (l2, Just val2) =
      if func val1 val2 == val1
        then (i1, Just val1)
        else (l2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS func (TS times values) =
  if all (== Nothing) values
    then Nothing
    else Just best
  where
    pairs = zip times values
    best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS times values) = TS times (Nothing : diffValues)
  where
    shiftValues = tail values
    diffValues = zipWith diffPair shiftValues values

--myMean :: (Real a) => [a] => Double
--myMean n list = sum list / n

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe values =
  if any (== Nothing) values
    then Nothing
    else (Just avg)
  where
    avg = mean (map fromJust values)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] _ = []
movingAvg values n =
  if length nextVals == n
    then meanMaybe nextVals : movingAvg restVals n
    else []
  where
    nextVals = take n values
    restVals = tail values

movingAvarageTS :: (Real a) => TS a -> Int -> TS Double
movingAvarageTS (TS [] []) n = TS [] []
movingAvarageTS (TS times values) n = TS times avgValues
  where
    ma = movingAvg values n
    nothings = replicate (n `div` 2) Nothing
    avgValues = mconcat [nothings, ma, nothings]
