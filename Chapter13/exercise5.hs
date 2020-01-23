import Parsing
import Control.Applicative

data Expr = Add Expr Expr | Times Expr Expr | Val Int deriving (Show, Read)

add :: Expr -> Parser Expr
add x = do symbol "+"
           y <- expr
           return (Add x y)

times :: Expr -> Parser Expr
times x = do symbol "*"
             y <- factor
             return (Times x y)

embedded :: Parser Expr
embedded = do symbol "("
              x <- expr
              symbol ")"
              return x

val :: Parser Expr
val = do x <- number
         return (Val x)

expr :: Parser Expr
expr = do x <- factor
          add x <|> return x

factor :: Parser Expr
factor = do x <- term
            times x <|> return x

term :: Parser Expr
term = embedded <|> val

eval :: String -> Expr
eval xs = case (parse expr xs) of
            [(n, [])] -> n
            [(_, out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid Input"
