

double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: Integer -> Integer
factorial n = product [1..n]

-- Need to add function declaration
average x = div (sum x) (length x)