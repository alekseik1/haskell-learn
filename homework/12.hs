euclid_ext x 0 
    | x >= 0 = (x, 1, 0)
    | otherwise = (-x, -1, 0)
euclid_ext a b = (d, x, y)
    where
    x = y'
    y = x' - y' * (a `div` b)
    (d, x', y') = euclid_ext b (a `mod` b)

-- *Main> euclid_ext 27 15
-- (3,-1,2)
-- *Main> euclid_ext (-5) (-2)
-- (1,-1,2)
-- *Main> euclid_ext (-5) 2
-- (1,1,3)
-- *Main> euclid_ext 5 (-2)
-- (1,-1,-3)
-- *Main> euclid_ext 5 2
-- (1,1,-2)
main = print "ok"
