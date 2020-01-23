-- From Haskell for Mac's tutorial page (not Graham Hutton's book).

-- Types with a fixed set of values are called Enums (think of C enums)
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Enum, Show)

-- Pattern Matching (nice)
isWeekday :: Day -> Bool
isWeekday Saturday = False
isWeekday Sunday = False
isWeekday _ = True

-- Switch Statement (not just if-else) - better than pattern matching
isWeekday' :: Day -> Bool
isWeekday' day = case day of 
    Saturday    -> False
    Sunday      -> False
    _           -> True

-- DATA CONSTRUCTORS act like VALUES, similar to 834 being a value of the Type Int.
weekends :: [Day] 
weekends = [Saturday, Sunday]

-- Since we've made Day a 'Type Class Instance' of Eq, we can compare them with == and /=.
sameDay :: Day -> Day -> Bool
sameDay day1 day2 = day1 == day2

nextDay :: Day -> Day
nextDay Sunday = Monday -- because Sunday is the 'end'
nextDay day = succ day

-- Another Example of Data Constructors acting like values (Could just be Strings, but not any random String)
data ProgrammingLanguages = Java | Python | C | CPP | Haskell | Bash | JavaScript deriving (Show, Enum)

addLanguage :: [ProgrammingLanguages] -> ProgrammingLanguages -> [ProgrammingLanguages]
addLanguage known new = new:known 

-- Another way to write Haskell Switch-Cases
isUseful :: ProgrammingLanguages -> Bool
isUseful lang = case lang of {Python -> True; C -> True; _ -> False}

-- Data Constructors and Type Constructors w/ parameters
-- We can have Data Constructors which take a parameter to produce the type: these ones have record syntax.
data ErrorCode = LogicalError {errorName :: String} | Success {result :: String} | SyntacticError {errorName :: String} deriving (Show) -- Only 3 error codes

-- We can create variables of the types
nullPointer :: ErrorCode
nullPointer = LogicalError "Null Pointer Error"

missingSemiColon :: ErrorCode
missingSemiColon = SyntacticError "Missing Semi-Colon (;)"

-- We can also create them via variables
createLogicalError :: String -> ErrorCode
createLogicalError name = LogicalError name

createSuccess :: String -> ErrorCode
createSuccess name = Success name

createSyntacticError :: String -> ErrorCode
createSyntacticError name = SyntacticError name

-- Redefine map using cases
map' :: (a -> b) -> [a] -> [b]
map' f xs = case xs of 
    [] -> []
    _ -> f ( head xs ) : map' f ( tail xs )