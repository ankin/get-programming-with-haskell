module Lesson15 where

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded, Eq)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum newPosition
  where
    halfAlphabet = alphabetSize `div` 2
    offset =
      if even alphabetSize
        then fromEnum c + halfAlphabet
        else 1 + fromEnum c + halfAlphabet
    newPosition = offset `mod` alphabetSize

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map func vals
  where
    alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    func = rotN alphabetSize

rotEncoder :: String -> String
rotEncoder input = map func input
  where
    alphabetSize = 1 + fromEnum (maxBound :: Char)
    func = rotN alphabetSize

rotDecoder = rotEncoder

xorBool :: Bool -> Bool -> Bool
xorBool val1 val2 = (val1 || val2) && not (val1 && val2)

xor :: [Bool] -> [Bool] -> [Bool]
xor vals1 vals2 = map xorPairs (zip vals1 vals2)
  where
    xorPairs (v1, v2) = xorBool v1 v2

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leading ++ reversed
  where
    reversed = reverse (intToBits' n)
    missingBits = maxBits - length reversed
    leading = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\(_, index) -> 2 ^ index) filtered)
  where
    size = length bits
    indices = [size -1, size -2 .. 0]
    filtered = filter (\(bit, _) -> bit == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

applyOTP' :: String -> String -> [Bits]
applyOTP' padText text =
  let textBits = map charToBits text
      padTextBits = map charToBits padText
      zipped = (textBits `zip` padTextBits)
   in map (\(b1, b2) -> b1 `xor` b2) zipped

applyOTP :: String -> String -> String
applyOTP padText text = map bitsToChar paddedBits
  where
    paddedBits = applyOTP' padText text

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OneTimePad String

instance Cipher OneTimePad where
  encode (OneTimePad pad) text = applyOTP pad text
  decode (OneTimePad pad) text = applyOTP pad text

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

--examplePrng :: Int -> Int
--examplePrng = prng 1337 7 100

createCycle :: (Int -> Int) -> Int -> [Int]
createCycle func seed = nextSeed : createCycle func nextSeed
  where
    nextSeed = func seed


--data StreamCipher = StreamCipher (Int -> Int)
--instance Cipher StreamCipher a where
--
--  examplePrng :: Int -> Int
--  examplePrng = prng 1337 7 100
--
--  padText :: () -> String
--  padText = (concatMap intToBits (createCycle examplePrng 12345))
--
--  encode pad

