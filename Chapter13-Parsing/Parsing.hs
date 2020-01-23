module Parsing where
  import Control.Applicative
  import Data.Char

  newtype Parser a = P (String -> [(a, String)])

  instance Functor Parser where
    --fmap :: Functor f => (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> [(g v, out)])

  instance Applicative Parser where
    --pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)]) -- produce a Parser with v as the result

    --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                         [] -> []
                         [(g, out)] -> parse (fmap g px) out) 

  instance Monad Parser where
    --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= g = P (\inp -> case parse px inp of
                          [] -> []
                          [(x, xs)] -> parse (g x) xs) -- parse the next character with Parser b

  instance Alternative Parser where
    --empty :: Parser a
    empty = P (\_ -> []) -- produce a failed Parser

    --(<|>) :: Parser a -> Parser a -> Parser a
    px <|> py = P(\inp -> case parse px inp of
                          [] -> parse py inp -- parse with px failed; parse with py
                          [(x, xs)] -> [(x, xs)])

    --many :: Parser a -> Parser a
    many p = some p <|> pure []

    --some :: Parser a -> Parser a
    some p = pure (:) <*> p <*> many p

  parse :: Parser a -> String -> [(a, String)]
  parse (P p) x = p x

  item :: Parser Char
  item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)])

  sat :: (Char -> Bool) -> Parser Char
  sat p = do x <- item
             if p x then return x else empty

  digit :: Parser Char
  digit = sat isDigit

  nat :: Parser Int
  nat = do x <- some digit
           return (read x)

  int :: Parser Int
  int = do char '-'
           x <- nat
           return (-x) <|> nat

  lower :: Parser Char
  lower = sat isLower

  upper :: Parser Char
  upper = sat isUpper

  letter :: Parser Char
  letter = sat isAlpha

  alphanum :: Parser Char
  alphanum = sat isAlphaNum

  ident :: Parser String
  ident = do x <- lower
             xs <- many alphanum
             return (x:xs)

  char :: Char -> Parser Char
  char x = sat (==x)

  string :: String -> Parser String
  string [] = return []
  string (x:xs) = do char x
                     string xs
                     return (x:xs)

  -- Skip spaces
  space :: Parser ()
  space = do many (sat isSpace)
             return ()

  -- get data between spaces
  token :: Parser a -> Parser a
  token p = do space
               v <- p
               space
               return v

  identifier :: Parser String
  identifier = token ident

  natural :: Parser Int
  natural = token nat

  integer :: Parser Int
  integer = token int

  number :: Parser Int
  number = natural <|> integer

  symbol :: String -> Parser String
  symbol s = token (string s)
