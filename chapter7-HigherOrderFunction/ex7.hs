-- Add a parity bit to the end of every byte in the binary string transmission algo.
-- A parity bit is 1 if the number of 1s is odd and 0 if the number of 1s is even.

import Data.Char

type Bit = Int

-- Convert a list of bits to base 10
bit2Int :: [Bit] -> Int
bit2Int binStr = foldr (\x -> \y -> x + 2*y) 0 binStr 

-- Convert an decimal int to a list of base-2 digitsss
int2Bit :: Int -> [Bit]
int2Bit 0 = []
int2Bit n = n `mod` 2 : int2Bit (n `div` 2)

-- Calculate parity value
parityVal :: [Bit] -> Bit
parityVal bits = if sum bits `mod` 2 == 0 then 0 else 1

-- Check if the parity value holds
ensureParity :: [[Bit]] -> Bool
ensureParity bytes = all (\byte -> ( sum (init byte) `mod` 2 ) == (last byte)  ) [byte | byte <- bytes ]

-- Ensures that a list of Bits is 8 Bits long
ensureByte :: [Bit] -> [Bit]
ensureByte bits = (take 8 (bits ++ repeat 0)) ++ [parityVal bits]

-- Split an list of bits into bytes
chopBytes :: [Bit] -> [[Bit]]
chopBytes [] = []
chopBytes bytes = take 9 bytes : chopBytes (drop 9 bytes)

-- encode a string into bits
encode :: String -> [Bit]
encode str = ( concat . map (ensureByte . int2Bit . ord) ) str

-- Return the given bits
channel :: [Bit] -> [Bit]
channel bit
    | (ensureParity . chopBytes) bit == True    = bit
    | otherwise                                 = []

-- Decode a list of bits into their string
decode :: [Bit] -> String
decode bits = ( map (chr . bit2Int) . chopBytes ) bits

-- Transmit a string as bits and decode it as a string.
transmit :: String -> String
transmit str = ( decode . channel . encode ) str