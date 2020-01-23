import Data.List

-- Given a list of numbers and operations in reverse polish notation, produce a result
solveRPN :: String -> Float
solveRPN expr = head ( foldl opExec [] ( words expr ) ) -- opExec tail head
    where   opExec stck@(x:y:xs) "+"            = (x + y):xs
            opExec stck@(x:y:xs) "-"            = (y - x):xs
            opExec stck@(x:y:xs) "*"            = (x * y):xs
            opExec stck@(x:y:xs) "/"            = (y / x):xs
            opExec stck@(x:y:xs) "^"            = (y ** x):xs
            opExec stck@(x:xs)  "ln"            = log x:xs
            opExec xs           "sum"           = [sum xs]
            opExec xs           numberString    = read numberString:xs