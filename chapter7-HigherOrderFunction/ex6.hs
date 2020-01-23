unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x 
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

int2Bin :: Int -> [Int]
int2Bin n = unfold (== 0) (`mod` 2) (`div` 2) n

-- Define chop8 using unfold
chop8 :: [Int] -> [[Int]]
chop8 bits = unfold (== []) (take 8) (drop 8) bits

-- Define map using unfold
map' :: Eq a => (a -> b) -> [a] -> [b]
map' f xs = unfold (== []) ( f.head ) tail xs -- f head xs : unfold p h t (tail xs)

-- Define iterate using unfold
-- iterate f x = [x, f x, f f x, f f f x ...]
iterate' :: (a -> a) -> a -> [a] 
iterate' f x = unfold (\_ -> False) (\x -> x) f x 