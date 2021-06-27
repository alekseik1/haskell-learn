data Compare a = Sg a | Mlt Int a deriving (Show)

-- b)
decompress' :: Compare a -> [a]
decompress' (Sg x) = [x]
decompress' (Mlt c x) = replicate c x

decompress :: [Compare a] -> [a]
decompress = concat . map decompress'

-- a)
compress' :: Eq a => [Compare a] -> a -> [Compare a]
compress' [] y  = [Sg y]
compress' ((Sg x) : xs) y
    | x == y = (Mlt 2 x):xs
    | otherwise = (Sg y):(Sg x):xs
compress' ((Mlt c x):xs) y
    | x == y = (Mlt (c + 1) x):xs
    | otherwise = (Sg y):(Mlt c x):xs

compress :: Eq a => [a] -> [Compare a]
compress = reverse . foldl compress' []

-- *Main> compress [1, 2, 3, 4, 4, 5, 0, 91, 12, 12]
-- [Sg 1,Sg 2,Sg 3,Mlt 2 4,Sg 5,Sg 0,Sg 91,Mlt 2 12]
-- *Main> decompress [Sg 1, Sg 2, Sg 3]
-- [1,2,3]
-- *Main> decompress [Sg 1, Sg 2, Sg 3, Mlt 2 11, Mlt 3 3]
-- [1,2,3,11,11,3,3,3]

main = print "ok"
