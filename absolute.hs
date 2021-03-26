-- Multiple type classes
absolute :: (Ord a, Num a) => a -> a
absolute x = if x >= 0 then x else -x
absolute' x
    | x >= 0 = x
    | otherwise = -x
