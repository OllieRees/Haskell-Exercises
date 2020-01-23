import System.Environment
import qualified Data.ByteString.Lazy as BS
import qualified Data.Word as Word

--  Cons' packs a word8 and adds it to the end of a bytestring; pack converts a list of word bytes to bytestring .
--  Cons creates a chunk for the appended word and a chunk for the rest of the list; cons' appends and then creates a chunk.
name :: BS.ByteString
name = BS.cons' 79 $ BS.pack [108, 108, 105, 101, 32, 82, 101, 101, 115]

-- Unpack turns a bytestring to a list of word bytes.
nameBytes :: [Word.Word8]
nameBytes = BS.unpack name

-- ByteString module contains a lot of stream-manipulating functions
copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents                    <-      BS.readFile source
    BS.writeFile   dest    contents

main = do
    args@(filename1:filename2:_)    <-      getArgs
    if          length args >= 2    then do
        copyFile    filename1   filename2
    else do
        return ()