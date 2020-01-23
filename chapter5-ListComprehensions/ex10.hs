-- Modify ceaser cipher to handle upper case letters

-- Create a ceaser cipher encoder and decoder

import Data.Char

expectedTable :: [Float]
expectedTable = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- Convert character to respective int
lowercase2Int :: Char -> Int
lowercase2Int c = ord c - ord 'a' -- ord c turns the character into the respective ascii val.

uppercase2Int :: Char -> Int
uppercase2Int c = ord c - ord 'A'

-- Convert int to respective character 
int2Lowercase :: Int -> Char
int2Lowercase i = chr (ord 'a' + i) -- chr turns the ascii val into the char.

int2Uppercase :: Int -> Char
int2Uppercase i = chr (ord 'A' + i)

-- Shift the characters forward by a factor
shift :: Char -> Int -> Char
shift c i 
    | isLower c = int2Lowercase (mod (lowercase2Int c + i) 26)
    | otherwise = int2Uppercase (mod (uppercase2Int c + i) 26)

-- Encode string
encode :: String -> Int -> String
encode str i = [shift c i | c <- str]

-- Percent calculator
percent' :: (Fractional a) => Int -> Int -> a
percent' num den = ((fromIntegral num) / (fromIntegral den)) * 100

-- Count the number of occurences of a character in a string
charCount :: Char -> String -> Int
charCount c str = sum [1 | x <- str, x == c]

-- Calculates the frequency each character appears in a string
freqString :: (Fractional a) => String -> [a]
freqString str = [percent' (charCount c str) strLength | c <- ['a'..'z'], c <- ['A' .. 'Z'] ]
    where 
        strLength = length [1 | c <- str, (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')]

-- Mathematical formula used to compare expected and observered character frequencies
chiSquare :: [Float] -> [Float] -> Float
chiSquare os es = sum [(oi - ei)^2/ei | (oi, ei) <- (zip os es)]

-- Shift list by n 
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- Decode encoded string
decode :: String -> String 
decode encodedStr = encode encodedStr (-factor)
        where
            factor = head ((\c -> \str -> [i | (x, i) <- zip str [0..], x == c]) (minimum chitab) chitab) -- Just used a lambda func here.
            chitab = [chiSquare (rotate n freqTable) expectedTable | n <- [0..25]]
            freqTable = freqString encodedStr