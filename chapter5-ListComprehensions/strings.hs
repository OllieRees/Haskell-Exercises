import Data.Char

-- Strings are a list of characters : [Char] == String
firstChar :: String -> Char
firstChar str = head str

-- We can use list comprehension on them
lowercaseNum :: String -> Int
lowercaseNum str = sum [1 | c <- str, c >= 'a' && c <= 'z']

uppercaseNum :: String -> Int
uppercaseNum str = length [c | c <- str, c >= 'A' && c <= 'Z']

splitCases :: String -> (String, String)
splitCases str = ([c | c <- str, c >= 'a' && c <= 'z'], [c | c <- str, c >= 'A' && c <= 'Z']) -- Could be improved

splitDigits :: String -> (String, [Int])
splitDigits str = ([c | c <- str, (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')], [digitToInt d | d <- str, d >= '0' && d <= '9'])

charCount :: String -> Char -> Int
charCount str c = sum [1 | x <- str, x == c]