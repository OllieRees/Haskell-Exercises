import System.IO
import System.Environment
import System.Directory
import Data.Char
import Data.List

-- Read file without withFile
rdFile :: String -> IO ()
rdFile filepath = do 
    handle          <-  openFile    filepath    ReadMode -- handle is the location of the file and the current line
    contents        <-  hGetContents handle 
    putStr          contents
    hClose          handle -- close the file (stop using it)

-- Read file with withFile
rdFile' :: String -> IO ()
rdFile' filepath = do
    withFile    filepath   ReadMode (\handle -> do
        contents    <-  hGetContents    handle
        putStr      contents
        )

-- Writes one line to a file
wrtFile :: String -> IO ()
wrtFile filepath = do
    withFile    filepath    WriteMode  (\handle -> do
        putStrLn    "Input line"
        line        <-  getLine   
        hPutStr     handle      line
        )

-- Read file with given buffer mode (NoBuffering LineBuffering BlockBuffering (Maybe Int))
rdFileBufferMode :: String -> BufferMode -> IO ()
rdFileBufferMode filepath mode = do
    withFile        filepath    ReadMode    (\handle -> do
        hSetBuffering   handle  mode
        contents        <- hGetContents handle
        putStr          contents
        )

-- Remove a line from a given file - user chooses the line to be removed
removeLine :: String -> IO ()
removeLine filepath = do
    handle                  <-  openFile    filepath   ReadMode
    (tempName, tempHandle)  <-  openTempFile "."     "temp"                                     -- crates tempfile temp.txt in curr.dir.
    contents                <-  hGetContents handle
    let listLines           =   lines contents
        numberedLines       =   zipWith (\n line -> show n ++ " - " ++ line) [0..] listLines
    putStrLn                "These are your lines: "
    putStr                  $ unlines numberedLines
    putStrLn                "Which one do you want to delete?"
    numberString            <- getLine
    let number              = read numberString                                                 -- read string as int
        newLines            = delete (listLines !! number) listLines                            -- delete the line at number from the lines
    hPutStr                 tempHandle $ unlines newLines                                       -- Print the file without the deleted line
    hClose                  handle
    hClose                  tempHandle
    removeFile              filepath                                                            -- Removes the given file from the hard drive
    renameFile              tempName    filepath                                                -- Renames temp.txt to the filename

-- Our own withFile function
wthFile' :: String -> IOMode -> (Handle -> IO a) -> IO a
wthFile' filepath editmode editfunc = do
    -- Create handle and pass it through the func.
    handle <- openFile filepath editmode
    result <- editfunc handle
    -- Close handle and return result
    hClose handle 
    return result

linesLessThanN :: Int -> IO ()
linesLessThanN n = interact $ unlines . filter (\xs -> length xs < n) . lines

getExecutionEnviron :: IO ()
getExecutionEnviron = do
    args        <- getArgs
    progname    <- getProgName
    putStrLn    ("Program name is " ++ progname)
    putStrLn    ("Arguments run on command line are : ") 
    mapM_ ( \args -> putStr (args ++ ", ") ) args
    putStrLn    ""

main = do
    getExecutionEnviron