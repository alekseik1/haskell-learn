-- a
head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x
-- b
tail' :: [a] -> [a]
tail' [] = error "Empty list"
tail' (x:xs) = xs
-- c
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs
-- d
drop' :: Int -> [a] -> [a]
drop' 0 list = list
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs
-- e
null' :: [a] -> Bool
null' [] = True
null' _ = False
-- f
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' elem (x:xs) 
    | x == elem = True
    | otherwise = elem' elem xs
-- g
-- at' :: [a] -> Int -> a
at' (x:xs) 0 = x
at' [] _ = error "index too large"
at' (x:xs) n = at' xs (n-1)
-- example; [1, 2, 3] `at'` 1
-- h
plusplus :: [a] -> [a] -> [a]
plusplus [] x = x
plusplus (x:xs) ys =  x : plusplus xs ys
-- i
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

main = print "ok"
