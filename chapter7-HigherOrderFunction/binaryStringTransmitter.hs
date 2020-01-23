-- Convert a string of decimal digits to a list of binary digits
-- Example from the book

import Data.Char

-- We can call an Int type, Bit now.
type Bit = Int

-- Convert a list of bits to base 10
bit2Int :: [Bit] -> Int
bit2Int binStr = foldr (\x y -> x + 2*y) 0 binStr

-- Convert an decimal int to a list of base-2 digitsss
int2Bit :: Int -> [Bit]
int2Bit 0 = []
int2Bit n = n `mod` 2 : int2Bit (n `div` 2)

-- Ensures that a list of Bits is 8 Bits long
ensureByte :: [Bit] -> [Bit]
ensureByte bits = take 8 (bits ++ repeat 0)

-- encode a string into bits
encode :: String -> [Bit]
encode str = ( concat . map (ensureByte . int2Bit . ord) ) str

-- Split an list of bits into bytes
chopBytes :: [Bit] -> [[Bit]]
chopBytes bytes = take 8 bytes : chopBytes (drop 8 bytes)

-- Decode a list of bits into their string
decode :: [Bit] -> String
decode bits = ( map (chr . bit2Int) . chopBytes ) bits

channel :: [Bit] -> [Bit]
channel bit = id bit

transmit :: String -> String
transmit str = ( decode . channel . encode ) str