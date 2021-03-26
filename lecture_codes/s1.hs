-- Eratosthenes' Sieve
sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [ y | y <- xs, y `mod` x /= 0]

primes = sieve [2..]

n1 = elem 91 primes
n2 = elem 101 primes

-- QuickSort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = lesser ++ [x] ++ greater
                where   lesser  = qsort [y | y <-xs, y <= x]
                        greater = qsort (filter (\z -> z > x) xs)

ns1 = qsort [4,5,1,7,12,6]
ns2 = qsort [3,2,3,3,1,5,0,2,1]
ns3 = qsort "What is your favorite PL?"

{-----------------------------------------}

-- 

abc = 5
ab_e = 7 :: Int
f1 = (\x y -> x) :: a -> b -> a
fNc n = (+) 3 n
fNb = \n -> (3 + n) `mod` 5
myList = [1,2,3] :: [Int]
f2 = f1 f1 (+) (*) div
n3' = f2 5 (-7)

-- f3 = \x -> x x
-- n4 = f2 5 -7
-- A = 7

--

d1 = head [1,2,3]
d2 = tail [1,2,3]
d3 = [[1,2,3] !! n | n <- [0..3]]
d4 = [1,2,3] !! 3
d5 = last [1,2,3]
d5_5 = init [1,2,3]
d6 = length [1,2,3]
d7 = 5 : [1,2,3]
d8 = 1 : 2 : 3 : []
d9 = [1,2,3] == d8
d10 = [1,2,3] ++ [4,5] ++ [6,7]
d11 = [1,2,3] ++ [4.5]
-- d12 = [1,2,3] ++ "abcde"
d13 = take 2 "abcde"
d14 = drop 2 "abcde"
d15 = reverse "abcde"
d16 = sum [1..9]

--

u = u

v = f1 u u

u1 = 1 / 0

v1 = 3 * u1
v2 = if 0 == 1 || 0 /= 0 then u1 else 2018

u2 = 2 : u2

v3 :: Int -> Int
v3 n = if n >= 0 then sum (take n u2) else error "negative value"

uN m = m : uN (m + 1)
sq_list1 = [n ^ 2 | n <- uN 0 ]
sq_list2 = map (\n -> n ^ 2) (uN 0)

b1 :: Int -> Bool
b1 n = take n sq_list1 == take n sq_list2

-- b2 = (\x -> x) == (\y -> y) :: Bool

--


