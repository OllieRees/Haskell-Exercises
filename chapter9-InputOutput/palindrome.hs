
main = do
    interact palindromeLines

palindromeLines :: String -> String
palindromeLines lns = ( unlines . map (\ln -> if isPalindrome ln then "Palindrome" else "Not Palindrome") . lines) lns 

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str