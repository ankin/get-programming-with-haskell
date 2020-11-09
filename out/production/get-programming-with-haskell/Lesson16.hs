module Lesson16 where

--data AuthorName = AuthorName
--  { firstName :: String,
--    lastName :: String
--  }

type FirstName = String

type MiddleName = String

type LastName = String

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName | TwoInitialsWithLast Char Char LastName deriving (Show)

data Author = Author Name deriving (Show)

data Artist = Person Name | Band String deriving (Show)

data Organization = Organization String deriving (Show)

data Creator = AuthorCreator Author | ArtistCreator Artist | Contact Organization deriving (Show)

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookName :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { toyName :: String,
    toyDescription :: String,
    toyPrice :: Double
  }

data Pamphlet = Pamphlet
  { pamphletTitle :: String,
    pamphletDescription :: String,
    pamphletContact :: Creator
  }

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy (PamphletItem pamphlet) = show (pamphletContact pamphlet)
madeBy _ = "unknown"

data Circle = Circle
  { radius :: Double
  }

data Square = Square
  { side :: Double
  }

data Rectangle = Rectangle
  { sideA :: Double,
    sideB :: Double
  }

data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter (CircleShape circle) = 2 * 3.14 * (radius circle)
perimeter (SquareShape square) = (side square) ^ 2
perimeter (RectangleShape rectangle) = (sideA rectangle) * (sideB rectangle)

