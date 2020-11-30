module Lesson22 where

import Control.Monad

myReplicateM :: Int -> IO String -> IO [String]
myReplicateM times func = mapM (\_ -> func) [1..times]

