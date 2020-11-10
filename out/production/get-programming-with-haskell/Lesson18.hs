module Lesson18 where

import qualified Data.Map as Map

data Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 0.2 0.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Philips" "Lovecraft"

first :: Triple a -> a
first (Triple a _ _) = a

second :: Triple a -> a
second (Triple _ b _) = b

third :: Triple a -> a
third (Triple _ _ c) = c

toList :: Triple a -> [a]
toList (Triple a b c) = [a, b, c]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple a b c ) = Triple (f a) (f b) (f c)

-- list impl in Haskell (GHCI> :info [])
-- data [] a = [] | a:[a]

data List a = Empty | Cons a (List a) deriving Show

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons head tail) = Cons (f head) (ourMap f tail)


-- tuple impl in Haskell (GHCi> :info (,))
-- data (,) a b = (,) a b

-- Kinds: types of types
-- The kind of a type indicates number of params the type takes
-- No params: *
-- One param: * -> *
-- Two params: * -> * -> *

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)