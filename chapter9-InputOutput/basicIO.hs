-- From learn you a haskell for the greater good
main = do 
    putStrLn "What's your name : "
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")