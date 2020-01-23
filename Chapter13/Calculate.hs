module Calculate where

import Parsing
import Control.Applicative

-- BNF of our parser
-- expr := term (+ expr | - expr | e)
-- term := factor (* term | / term | e)
-- factor := (expr) | int
-- int := .. | -1 | 0 | 1 | ..

add :: Int -> Parser Int
add x = do symbol "+"
           y <- expr
           return (x + y)

sub :: Int -> Parser Int
sub x = do symbol "-"
           y <- expr
           return (x - y)

mul :: Int -> Parser Int
mul x = do symbol "*"
           y <- term
           return (x * y)

divi :: Int -> Parser Int
divi x = do symbol "/"
            y <- term
            return (x `div` y)

paren :: Parser Int
paren = do symbol "("
           x <- expr
           symbol ")"
           return x

exponen :: Int -> Parser Int
exponen x = do symbol "^"
               exp <- number
               return (x^exp)

expr :: Parser Int
expr = do x <- term
          add x <|> sub x <|> return x

term :: Parser Int
term = do x <- expoLayer
          mul x <|> divi x <|> return x

expoLayer :: Parser Int
expoLayer = do x <- factor
               exponen x <|> return x

factor :: Parser Int
factor = paren <|> number

