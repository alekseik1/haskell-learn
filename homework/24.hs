takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x acc -> if f x then x : acc else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f = foldl (\acc x -> if (f x) && (length acc == 0) then [] else acc ++ [x]) []
-- *Main> dropWhile' (<3) [1, 2, 3, 4, 5]
-- [3,4,5]
-- *Main> dropWhile' (<3) [1, 2, 3, 2, 1]
-- [3,2,1]
-- *Main>

main = print "ok"
