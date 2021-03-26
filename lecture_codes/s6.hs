map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x :  map' f xs

b0 f n = take n (map f [0..]) == take n (map' f [0..]) 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : ys else ys
        where ys = filter' p xs

l0 = filter even (map (\x->x^2) [0..100])

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) | p x         = all' p xs
              | otherwise   = False

b1 = all' even (map (2*) [0..100])
b2 = all' even [0..100]
b3 = any even [0..100]

isPrime :: Int -> Bool
isPrime n = n > 1 && and [ n `mod` x /= 0 | x <- [2..(n-1)] ]

l1 = takeWhile isPrime (filter odd [2..])
l2 = dropWhile (\x -> x < 10) [0..100]
l3 = dropWhile isPrime [2,3,5,19,7,23,91,17,13,31]

--

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f x [] = x
foldr' f x (y:ys) = f y (foldr' f x ys)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f x [] = x
foldl' f x (y:ys) = foldl' f (f x y)  ys

l4 = [1,2,3]

n0 = foldr' (+) 0 l4
n1 = foldl' (+) 0 l4

n2 = foldr (\u v -> u^v) 2 l4
n3 = foldl (\u v -> u^v) 2 l4

l5 = scanr (\u v -> u^v) 2 l4
l6 = scanl (\u v -> u^v) 2 l4

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f x [] = [x]
scanr' f x (y:ys) = f y (head z) : z
                where z = scanr' f x ys

listid :: [a] -> [a]
listid = foldr (:) []

conc' :: [a] -> [a] -> [a]
conc' ys zs   = foldr (:) zs ys

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\u v -> f u : v) []

sffx :: [a] -> [[a]]
sffx = scanr (:) []

--

l7 = take 10 (iterate (^2) 3)
l8 = take 10 (iterate (*2) 1)

iterate' :: (a -> a) -> a -> [a]
iterate' f x =  scanl (\u _ -> f u) x [0..]

-- replicate; repeat
--

type Bin = [Int]

bin2int :: Bin -> Int
bin2int bits = foldl (\x y -> 2*x + y) 0 bits

int2bin :: Int -> Bin
int2bin 0 = []
int2bin n = (int2bin (n `div` 2)) ++ [n `mod` 2]

{-- Remove consecutive duplicates from a list --}

duprem :: (Eq a) => [a] -> [a]
duprem = foldr (\u vs -> if null vs || u /= head vs then u : vs else vs) []

{-- Group consecutive duplicates together  --}

grp :: (Eq a) => a -> [[a]] -> [[a]]
grp x [] = [[x]]
grp x (ls:lss) | x == head ls   = (x : ls) : lss
               | otherwise      = [x] : ls : lss

dupgrp :: (Eq a) => [a] -> [[a]]
dupgrp = foldr grp []

--

comp' :: (b -> c) -> (a -> b) -> a -> c
comp' g f x  = g (f x)

odd' = not . even

f1 :: [Int] -> Int
f1 =  sum . (++) [1,2,3] . map (^2) . filter even

itcomp :: Int -> (a -> a) -> (a -> a)
itcomp 0 f = id
itcomp n f = f . (itcomp (n - 1) f)

--itcomp' :: Int -> (a -> a) -> (a -> a)
itcomp' n f = (iterate ((.) f) id) !! n

compose :: [a -> a] -> a -> a
compose = foldr (.) id

itcomp'' n  = compose . (replicate n)

--

