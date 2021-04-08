-- a
euclid_div x 0 = x
euclid_div x y = euclid_div y (x `mod` y)
-- b
euclid_sub x 0 = x
euclid_sub x y | x > y = euclid_sub y (x - y)
               | otherwise = euclid_sub x (y - x)

main = print "ok"
