even' :: Integral a => a -> Bool
odd' :: Integral a => a -> Bool

even' 0 = True
even' 1 = False
even' x = odd' (x-1)
odd' 0 = False
odd' 1 = True
odd' x = even' (x-1)


evenSum :: [Integer] -> Integer
evenSum [] = 0
evenSum (x:xs)
    | even' x = x + evenSum(xs)
    | otherwise = evenSum(xs)

evenSum' l = foldl (+) 0 (filter even l)
