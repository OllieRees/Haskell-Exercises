main = do
    line <- getLine
    if null line then
        return ()
    else do
        putStrLn $ reverseLine line
        main

reverseLine :: String -> String
reverseLine ln = (unwords . map reverse . words) ln