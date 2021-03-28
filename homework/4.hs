replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x
    | n >= 0 = x : replicate (n-1) x
    | otherwise = []

repeat' :: a -> [a]
repeat' x = x : repeat x

main = print ("ok")
