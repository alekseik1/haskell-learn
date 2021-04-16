get_divisors :: Int -> [Int]
get_divisors n = [x | x <- [1..(n-1)], mod n x == 0]
-- are_friendly :: Int -> Int -> Bool
are_friendly x y = sum (get_divisors x) == y && sum (get_divisors y) == x

main = print "ok"
