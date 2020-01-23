import System.IO
import System.Environment
import System.Directory
import Data.List

add :: [String] -> IO ()
add [filepath, line] = do
    appendFile filepath (line ++ "\n")

view :: [String] -> IO ()
view [filepath] = do
    contents        <-  readFile    filepath
    let linesList   =   lines       contents
        lineNums    =   zipWith     (\n line -> show n ++ "-" ++ line) [0..] linesList
    putStr          $   unlines     lineNums

remove :: [String] -> IO()
remove [filepath, linenumStr] = do
    handle                          <-  openFile        filepath                ReadMode
    (tempname, temphandle)          <-  openTempFile    "."                     "temp"
    contents                        <-  hGetContents    handle
    let number                      =   read            linenumStr
        oldLines                    =   lines           contents
        newLines                    =   delete          (oldLines !! number)    oldLines
    hPutStr                         temphandle          $ unlines               newLines
    hClose                          handle
    hClose                          temphandle
    removeFile                      filepath
    renameFile                      tempname            filepath

-- bump line to first 
bump :: [String] -> IO ()
bump [filepath, bumpedLinenumStr] = do

    -- Initialise 
    handle                          <-  openFile        filepath                ReadMode
    (tempname, temphandle)          <-  openTempFile    "."                     "temp"
    contents                        <-  hGetContents    handle

    -- Get the line to be be bumped and add it to temp file
    let number                      =   read            bumpedLinenumStr   ::      Int
        oldLines                    =   lines           contents
        bumpLine                    =   (oldLines !! number)

    -- Get the rest of the lines
    let restofLines                 = filter            (/= bumpLine)           $ lines contents                          
    
    -- append bumpLine to the beginning of restofLines and add it to tempname
    hPutStr                         temphandle          $ unlines (bumpLine:restofLines)
    
    -- Finalise
    hClose                          handle
    hClose                          temphandle
    removeFile                      filepath
    renameFile                      tempname            filepath


dispatch :: [(String, [String] -> IO ())]
dispatch =  [       ("add", add),
                    ("view", view),
                    ("remove", remove),
                    ("bump", bump)
            ]

-- first argument must always be the action within the dispatch
main = do
    (command:args)      <- getArgs
    let (Just action)   = lookup command dispatch -- looksup the given command in the dispatch list
    action args