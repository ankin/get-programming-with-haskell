module Lesson13 where

class Describable a where
  describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == minBound
    then n
    else succ n


