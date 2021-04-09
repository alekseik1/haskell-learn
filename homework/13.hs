euclid_ext x 0 
    | x >= 0 = (x, 1, 0)
    | otherwise = (-x, -1, 0)
euclid_ext a b = (d, x, y)
    where
    x = y'
    y = x' - y' * (a `div` b)
    (d, x', y') = euclid_ext b (a `mod` b)
---------
integers = 0 : concat [[x,(-x)] | x <- [1..]]
solve :: (Int, Int, Int) -> [(Int, Int)]
solve (a, b, c)
    | c `mod` d == 0 = [(u0 - b' * t, v0 + a' * t) | t <- integers]
    | otherwise = []
    where
    d = gcd a b
    a' = a `div` d
    b' = b `div` d
    c' = c `div` d
    (_, u0', v0') = euclid_ext a' b'
    u0 = u0' * c'
    v0 = v0' * c'

main = print "ok"
