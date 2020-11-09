module Lesson7 where

myGCG a b =
  if remainder == 0
    then b
    else myGCG b remainder
  where
    remainder = a `mod` b

myGCG2 a b = case remainder of
  0 -> b
  _ -> myGCG b remainder
  where
    remainder = a `mod` b