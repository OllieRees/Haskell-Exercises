import System.IO
import Control.Applicative
import Parsing
import Calculate

-- UI Functions
showBox :: IO ()
showBox = sequence_ [writeAt (1, y) b | (b, y) <- zip box [1..]]

box :: [String]
box = ["+--------------+",
      "|               |",
      "+---------------+",
      "| q | c | d | = |",
      "+---------------+", 
      "| 1 | 2 | 3 | + |",
      "+---------------+", 
      "| 4 | 5 | 6 | - |",
      "+---------------+", 
      "| 7 | 8 | 9 | * |", 
      "+---------------+", 
      "| 0 | ( | ) | / |", 
      "+---------------+"]

buttons :: String
buttons = "qcd=123+456-789*0()/QCD \ESC\BS\DEL\n"

-- Display string in the calculator's display box
display :: String -> IO ()
display xs = do 
  writeAt (2, 2) (replicate 13 ' ' )
  writeAt (2, 2) (reverse (take 13 $ reverse xs))

-- Behavioural Functions
calculate :: String -> IO ()
calculate xs = do
  display xs
  c <- getCh
  if c `elem` buttons then
                      process c xs
                      else
                      do beep
                         calculate xs

-- Process what the character's purpose is
process :: Char -> String -> IO ()
process c xs
  | c `elem` "qQ\ESC" = quit
  | c `elem` "cC" = clear
  | c `elem` "dD\BS\DEL" = backspace xs
  | c `elem` "=\n" = eval xs
  | otherwise = press c xs


-- Button Functions
-- Quit program
quit :: IO ()
quit = goto (1, 14)

-- Clear the display
clear :: IO ()
clear = calculate []

-- Undo previous character
backspace :: String -> IO ()
backspace [] = calculate []
backspace xs = calculate (init xs)

-- Evaluate the parsed display string
eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, [])] -> calculate (show n)
            _         -> do beep
                            calculate xs

-- Add c onto xs in the display
press :: Char -> String -> IO ()
press c xs = calculate (xs ++ [c])

--Clears Screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- Beeps
beep = putStr "\BEL"

-- Move cursor to position (x, y)
goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

--Reads a single char and doesn't print it to the screen
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- Writes String at given position
writeAt :: (Int, Int) -> String -> IO ()
writeAt p xs = do 
  goto p
  putStr xs

-- run routine: clear screen, show UI and run calculate []
run :: IO ()
run = do cls
         showBox
         calculate []
