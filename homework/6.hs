get_divisors :: Int -> [Int]
get_divisors n = [1] ++ [x | x <- [1..(n-1)], y <- [1..(n-1)], x*y == n]
-- are_friendly :: Int -> Int -> Bool
are_friendly x y = sum (get_divisors x) == y

main = print "ok"
