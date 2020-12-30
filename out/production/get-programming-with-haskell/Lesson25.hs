{-# LANGUAGE OverloadedStrings #-}

module Lesson25 where

import System.Environment
import qualified Data.ByteString.Char8 as BC

glitchFile :: IO ()
glitchFile = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  let glitched = imageFile -- glitching function to follow
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack

intToChar :: Int -> Char
intToChar int = toEnum safeInt
                where safeInt `mod` 255

