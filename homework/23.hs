-- a) map;
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\y ys -> (f y):ys) [] xs
-- *Main> map' ((*) 2) [1, 2, 3]
-- [2,4,6]

-- b) filter;
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\y ys -> if f y then y:ys else ys) [] xs
-- *Main> filter'  (\x -> x `mod` 2 == 0) [1..10]
-- [2,4,6,8,10]

-- c) and;
and' :: [Bool] -> Bool
and' l = foldl (\y ys -> if y then ys else False) True l
-- *Main> and' [True]
-- True
-- *Main> and' [True, False]
-- False
-- *Main> and' [True, False, True]
-- False
-- *Main> and' [True, True, True]
-- True

-- d) or;
or' :: [Bool] -> Bool
or' l = foldl (\y ys -> if y then True else ys) False l
-- *Main> or' [True, False, True]
-- True
-- *Main> or' [True, False, False]
-- True
-- *Main> or' [False, False, False]
-- False
-- *Main> or' [True]
-- True
-- *Main> or' [False]
-- False

-- e) all; (постарайтесь не использовать ни одной локальной переменной
-- типа элемента списка, а применять оператор композиции)
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr ((&&) . f) True
-- *Main> all' (>10) [11, 11, 12]
-- True
-- *Main> all' (>10) [11, 12, 13]
-- True
-- *Main> all' (>11) [11, 12, 13]
-- False
-- *Main> all' (>5) [11, 12, 13]
-- True

-- f) any. (то же)
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr ((||) . f) False
-- *Main> any' (>5) [11, 4, 3]
-- True
-- *Main> any' (>5) [5, 4, 3]
-- False
-- *Main> any' (>5) [10, 12, 12]
-- True
-- *Main> any' (>5) [1, 2, 3]
-- False

main = print "ok"
