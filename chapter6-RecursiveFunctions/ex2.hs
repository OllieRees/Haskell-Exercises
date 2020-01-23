-- Define a recursive function that returns the sum of [n, (n - 1)..0]
sumDown :: Int -> Int
sumDown 0 = 0
sumDown n = n + sumDown(n - 1)