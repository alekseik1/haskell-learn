--

fac_a :: Int -> Int
-- The clause order is essential since the first wins,
-- so we do not go below zero.
fac_a 0 = 1
fac_a n = n * fac_a (n-1)

fac_b :: Int -> Int
fac_b n | n > 0      = n * fac_b (n-1)
        | otherwise  = 1

binom :: Int -> Int -> Int
binom _ 0 = 1
binom 0 _ = 0
binom n k = binom (n - 1)  k + binom (n - 1) (k - 1)

acker :: Integer -> Integer -> Integer
acker 0 n = n + 1
acker m 0 = acker (m - 1) 1
acker m n = acker (m - 1) (acker m (n - 1))

bracket :: Integer -> Integer
bracket n | odd n        = 0
          | n == 0       = 1
          | otherwise    = sum [ bracket (i*2) * bracket (m - (i*2)) 
                                | let m = n - 2, i <- [0 .. (m `div` 2)] ]

catalan n = bracket (n * 2)

--

f1 :: Int -> Int
f1 n = f1 (n + 1)

--

even' :: Int -> Bool
odd' :: Int -> Bool

even' 0 = True
even' n = odd' (n - 1)

odd' 0 = False
odd' n = even' (n - 1)

--

length' :: [a] -> Int
length' [] = 0
-- (:) binds weaker than a function application
length' (x : xs) = 1 + length' xs

-- (:) binds stronger than =
last' :: [a] -> a
last' [] = error "Empty list!"
last' [x] = x
last' (_:xs) = last' xs  


init' :: [a] -> [a]
init' [] = []
init' (x:xs) | null xs    = []
             | otherwise  = x : init' xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

--

{-- Bubble sorting --}

bubble :: (Ord a) => a -> [a] -> [a]
bubble x [] = [x]
bubble x (y:ys) | x <= y        = x : (bubble y ys)
                | otherwise     = y : (bubble x ys)

isSort :: (Ord a) => [a] -> Bool
isSort xs = and [ x <= y | (x,y) <- zip xs (tail xs) ]

bubsort :: (Ord a) => [a] -> [a]
bubsort xs = if isSort xs then
                xs
             else
                bubsort (bubble (head xs) (tail xs))

{-- Insertion sorting --}

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : (insert x ys)

inssort :: (Ord a) => [a] -> [a]
inssort [] = []
inssort (x:xs) = insert x (inssort xs)

{-- Merge sorting --}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y     = x : merge xs (y:ys)
                    | otherwise  = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take l xs, drop l xs)
   where l = div (length xs) 2

mrgsort :: Ord a => [a] -> [a]
mrgsort xs | length xs < 2  = xs
           | otherwise      = merge (mrgsort(fst(halve xs))) 
                                (mrgsort(snd(halve xs)))

{-- Quick sorting --}

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = lesser ++ [x] ++ greater
                where   lesser  = qsort [y | y <-xs, y <= x]
                        greater = qsort [y | y <-xs, y > x]


{-- Largest common subsequence via Dynamic Programming --}

lcs' :: (Eq a) => [a] -> [a] -> Int -> Int -> [a]
lcs' xs ys i j | i < 0 || j < 0    = []
              | x == y              = x : lcs' xs ys (i - 1) (j - 1)
              | otherwise           = if (length u >= length v) then
                                            u
                                        else
                                            v
              where x = xs !! i; y = ys !! j
                    u = lcs' xs ys i (j - 1)
                    v = lcs' xs ys (i - 1) j

lcs :: (Eq a) => [a] -> [a] -> [a]
lcs xs ys = reverse (lcs' xs ys (length xs - 1) (length ys - 1))



