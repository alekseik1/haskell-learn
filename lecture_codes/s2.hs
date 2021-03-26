import GHC.Float
-- for float2Double

-- import Data.Either
-- for Either magic at the end

--

x = True :: Bool
y = False

-- Multiple declarations are not allowed.
--x = True

z = if (&&) ((x || (not y)) && x) (not y) then x else y

myAnd = \x y -> if x then y else x

x' = True `myAnd` False :: Bool

--

c1 = 'H' :: Char
c2 = '\n' :: Char
v0 = putChar c2

--

n0 = 3 :: Int
n1 = (-2)
n2 = n0 + n1

n3 = 2^63
n4 = 2^63 :: Int
-- What is the type of n3?

n3' = 2^63
n4' = 2^63 :: Int
-- What is the type of n3'?
n5 = n3' + n4'

{- NOTE on Int overflow.
 - Let Int be the range [-M..M-1] (currently M = 2^63). Let L = 2M.
 - On overflow, the value 'wraps around', so M is mapped to -M, M+1 to
 - (-M-1) etc. Thus, for any x \in \Z one has
 -      I(x) = (x + M) mod L - M,
 - where I(x) :: Int is an Int presentation of x, and all arithmetical
 - operations are 'absolute' (i.e. performed in \Z). Obiously, I(x) = x
 - for x :: Int. Hence, I(I(x)) = I(x) for all x \in \Z.
 -
 - E.g., one obtains I(2^63) = I(M) = (2M) mod (2M) - M = -M, as is the
 - case. For x = -M + (-M), we get I(x) = (-M) mod (2M) - M = M - M = 0.
 -
 - It makes no difference whether summands 'wrap around' before addition,
 - if the sum does. So one has
 -      I(x + y) = I(I(x) + I(y))
 - for all x, y \in \Z. Indeed, I(I(x) + I(y)) = ((x + M) mod L +
 - (y + M) mod L - M) mod L - M = ((x + y + 2M) mod L - M) mod L - M.
 - Note that 2M mod L = 0 and -M mod L = M. Thus,
 -  I(I(x) + I(y)) = ((x + y) mod L - (-M) mod L) mod L - M =
 -      = ((x + y + M) mod L) mod L - M = ((x + y) + M) mod L - M =
 -          = I(x + y).
 - Also, it is easy to see that
 -      I(x * y) = I(I(x) * I(y))
 -}

n6 = 10 :: Int
n7 = 11 :: Integer
--n8' = n6 + n7
n8 = n6 + fromIntegral(n7)

fl = pi :: Float
db = pi :: Double

--v1 = fl + n6
--v2 = fl * db

v3 = db + fromIntegral(n6)
v4 = float2Double(fl) * db 

v5 = floor(sqrt(2))

--

p0 = (True,(12,"Haskell B. Curry")) :: (Bool,(Int,String))
p1 = fst p0
p2 = fst (snd p0)
p3 = snd (snd p0)

--

e0 = [] :: [b]
e1 = [] :: (Eq a) => [a]
e2 = [] 
e3 = [] :: [Int]

bl0 = e0 == e1
--b0' = e0 == e2
bl0'' = e1 == e2
bl1 = e2 == e3
bl2 = e1 == e3

bools = [True,False,True] :: [Bool]
nums = [[],[1,2,3],[7]] :: [[Int]]
chars = ['a','b','c'] :: [Char]

s1 = "Qwerty!" :: String
s2 = "Qwerty!"
s3 = ['Q','w','e','r','t','y','!']

bl3 = s1 == s2 && s2 == s3

l1 = [(False,'O'),(True,'1')] :: [(Bool,Char)]

p5 = ([False,True],['0','1']) :: ([Bool],[Char])

-- What is the most general type for l2?
l2 = [\x y-> x, \x y -> y]

l3 = [tail,init,reverse] :: [[a] -> [a]]

--

bl4 = (1,2) < (1,3)

bl5 = "Are" < "Arena"

s4 = show 15
--n10' = read "16"
n10 = read "16"

n11 = n10 - read s4

{-- Num, Integral, Fractional--}

data Z2 = Z | U
    deriving (Eq, Ord, Show, Read)

bl6 = Z == U
bl7 = Z < U

instance Num Z2 where
    Z + Z = Z
    U + Z = U
    Z + U = U
    U + U = Z
    _ * Z = Z
    Z * _ = Z
    U * U = U
    negate = id
    abs = id
    signum = id
    fromInteger n = if mod n 2 == 0 then Z else U

x1 = Z :: Z2
x2 = U :: Z2

{---------------------------------------------------}

{- QUESTION: Is it possible to use if-then-else in
 - such a way that the 'branches' have arbitrary types
 - without any obvious 'general unifier'?
 -
 - Let's try Either type!
-} 

-- Some arbitrary types...
data Foo = Qwerty
    deriving (Show)
data Bar = Asdf
    deriving (Show)

xx = Qwerty :: Foo
yy = Asdf :: Bar

-- 'Encapsulate' xx and yy into Either Foo Bar
xx' = Left xx :: Either Foo Bar
yy' = Right yy :: Either Foo Bar

zz' = (\bl -> if bl then xx' else yy') :: Bool -> Either Foo Bar

-- Now we want to treat zz' differently whether it is xx' or yy'.
-- Of course, at some stage we must return the values of the same type.

funx = (\_ -> "Yea, yea") :: Foo -> String
funy = (\_ -> "Nay, nay") :: Bar -> String


zz = (\bl -> either funx funy (zz' bl)) :: Bool -> String

sx = zz True
sy = zz False

--
