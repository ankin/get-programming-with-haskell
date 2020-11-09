module Lesson4 where

ifEven myFunc x =
  if even x
    then myFunc x
    else x

genIfEven f = (\x -> ifEven f x)

genIfXEven x = (\f -> ifEven f x)

names = [("Tod", "Smith"), ("John", "Smith"), ("Bernard", "Gravesohn")]

compareLastNames name1 name2 =
  let lastName1 = snd name1
      lastName2 = snd name2
      firstName1 = fst name1
      firstName2 = fst name2
   in compareAny lastName1 lastName2 (compareAny firstName1 firstName2 EQ)

compareAny n1 n2 func
  | n1 > n2 = GT
  | n1 < n2 = LT
  | otherwise = func

