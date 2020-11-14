module Lesson19 where

import qualified Data.Map as Map
import Data.Maybe (isNothing)

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]
ids = [1, 7, 13, 14, 21, 24]

organPairs = zip ids organs

organCatalog = Map.fromList organPairs

possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents drawers catalog = map lookupOrgan drawers
  where lookupOrgan = (\drawer -> Map.lookup drawer catalog)

availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan:: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (\o -> o == Just organ) available)


isSomething :: Maybe Organ -> Bool
isSomething (Just _) = True
isSomething Nothing = False


showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""


numOrZero :: Maybe Int -> Int
numOrZero (Just value) = value
numOrZero Nothing = 0

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in vat"
  show (Cooler organ) = show organ ++ " in cooler"
  show (Bag organ) = show organ ++ " in bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process:: Maybe Organ -> Maybe (Location, Container)
process (Just organ) = Just (placeInLocation (organToContainer organ))
process Nothing = Nothing


report :: Maybe (Location, Container) -> String
report (Just (location, container)) = show container ++ " in the " ++ show location
report Nothing = "Error, organ not found"

processAndReport ::  Maybe Organ -> String
processAndReport organ = report (process organ)

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport (Map.lookup id catalog)

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers mOrgans = length (filter isNothing mOrgans)
-- emptyDrawers mOrgans = length (filter (not . isSomething) mOrgans)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap func (Just a)= Just (func a)
maybeMap _ Nothing = Nothing