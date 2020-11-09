module Lesson17 where


myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Transparent deriving (Show, Eq)

instance Semigroup Color where
  (<>) Transparent col = col
  (<>) col Transparent = col
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Red Yellow = Orange
  (<>) Yellow Red = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

instance Monoid Color where
  mempty = Transparent
  mappend = (<>)

-- Events and Probabilities

data Events = Events [String]

data Probs = Probs [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
  where
    totalProbs = sum probs
    normalizedProbs = map (\prob -> prob / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

instance Semigroup Events where
  (<>) events1 (Events []) = events1
  (<>) (Events []) events2 = events2
  (<>) (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where
      combiner = (\a b -> mconcat [a, "-", b])

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

--combineEvents :: Events -> Events -> Events
--combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
--  where
--    combiner = (\a b -> mconcat [a, "-", b])

instance Semigroup Probs where
  (<>) probs1 (Probs []) = probs1
  (<>) (Probs []) probs2 = probs2
  (<>) (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

--combineProbs :: Probs -> Probs -> Probs
--combineProbs (Probs probs1) (Probs probs2) = Probs (cartCombine (*) probs1 probs2)

instance Semigroup PTable where
  (<>) ptable (PTable (Events []) (Probs [])) = ptable
  (<>) (PTable (Events []) (Probs [])) ptable = ptable
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = e1 <> e2
      newProbs = p1 <> p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)
