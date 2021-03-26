-- For guard :: Bool -> f ()
import Control.Monad

-- For ord :: Char -> Int
import Data.Char

l0 = [1,2,3,4,5]

l1 = [x^2 | x <- l0]

l2 = [x^2 | x <- [1..]]

l2' = take 100 [(x,y) | x <- l0, y <- [1..]]
l2'' = take 100 [(x,y) | y <- [1..], x <- l0]

l3 = [(x,y,z) | x <- l0, y <- l0, let z = x + y]

l4 = [(x,y,z) | x <- l0, y <- l0, let z = x + y, even z]

l5 = do x <- l0
        y <- l0
        let z = x + y
        guard (even z)
        return (x,y,z)

l6 = [ z | (_,_,z) <- l3 ]

f1 :: Int -> [(Int,Int)]
f1 n = [ (x,y) | x <- [0..n], y <- [0..x] ]

f2 :: Int -> [[(Int, Int)]]
f2 n = [ [ (x,y) | y <- [0..x] ] | x <- [0..n] ]

f3 :: Int -> [(Int,Int)]
f3 = concat . f2

----

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

primes :: [Int]
primes = [x | x <- [2..], length (factors x) == 2 ]

nPrime n = primes !! n

perfects :: [Int]
perfects = [x | x <- [1..], 2*x == sum (factors x)]

--
--

l7 = zip l0 l1
l7' = zip l1 l2

positions :: (Eq a) => a -> [a] -> [Integer]
positions x xs = [i | (y,i) <- zip xs [0..], y == x]

scalprod :: [Int] -> [Int] -> Int
scalprod xs ys = sum [ a*b | (a,b)  <- (zip xs ys)]

twins :: [(Int,Int)]
twins = [ (p,q) | (p,q) <- zip primes (tail primes), q == p + 2 ]
 
--

let2int :: Char -> Int
let2int c = ord c - ord ' '

int2let :: Int -> Char
int2let n = chr (n + ord ' ')

shift :: Int -> Char -> Char
shift n c = int2let (((let2int c) + n) `mod` (ord '~' - ord ' ' + 1))

encode ::  Int -> String -> String
encode n cs = [shift n c | c <- cs]

decode ::  Int -> String -> String
decode n cs = [shift (-n) c | c <- cs]

--


