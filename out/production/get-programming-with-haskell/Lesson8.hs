module Lesson8 where

length2 [] = 0
length2 [x] = 1
length2 (x : xs) = 1 + length2 xs

take2 _ [] = []
take2 0 _ = []
take2 n (x : xs) = x : take2 (n - 1) xs

collatz 1 = 1
collatz n =
  if even n
    then collatz 1 + (n `div` 2)
    else collatz 1 + (n * 3 + 1)

