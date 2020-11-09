module Lesson9 where

reverse2 [] = []
reverse2 [x] = [x]
reverse2 aList = rev aList []
  where
    rev [] el = el
    rev (x : xs) el = rev xs (x : el)

reverse3 aList = foldl rcons [] aList
  where
    rcons x y = y : x

elem2 el aList = length filtered > 0
  where
    filtered = filter (== el) aList

