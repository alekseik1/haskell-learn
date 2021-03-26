--

type R3 = (Float,Float,Float)


type Array a = [a]

myTail :: Array a -> [a]
myTail [] = []
myTail (_:xs) = xs

type Assoc k v = [(k,v)]

find :: Eq k =>  Assoc k v -> k -> v
find [] x = error "Key not found"
find ((z,z') : zs) x | z == x        = z'
                     | otherwise     = find zs x

fl = (pi,exp(1),exp(pi)) :: R3
b0 = fl == fl

{--
instance Eq R3 where
    (x,y,z) == (x',y',z') = (x == x') && (y == y') && (z == z')
--}

--

data Day = Monday
          | Tuesday | Wednesday
          | Thursday | Friday
          | Saturday | Sunday
          deriving (Eq,Ord,Show,Read)

today = Monday
b1 = Tuesday < Friday

next :: Day -> Day
next x = case x of
            Monday -> Tuesday
            Tuesday -> Wednesday
            Wednesday -> Thursday
            Thursday -> Friday
            Friday -> Saturday
            Saturday -> Sunday
            Sunday -> Monday

comp 0 f = id
comp n f = f . (comp (n - 1) f)

prev :: Day -> Day
prev = comp 6 next

d1 = next today
d2 = prev today

--

data Date = Dt (Int,Int,Int)
        deriving (Eq)

d3 = Dt (2,4,2018)
d4 = Dt (2,4,2017)

b2 = d3 == d4
b3 = d3 == d3

instance Show Date where
    show (Dt (d,m,y)) = (if d < 10 then "0" else "")
                      ++ show d ++ "."
                      ++ (if m < 10 then "0" else "")
                      ++ show m ++ "." ++ show y
                      
s1 = show d3

-- An alterenative 'struct' style for many-argument constructors
data Date'' = Dt'' { day :: Int, month :: Int, year :: Int }
              deriving (Eq,Show)

d101 = Dt'' 16 4 2019
m101 = month d101
d102 =  Dt'' {year = 1861, day = 11, month = 02}

--

newtype Date' = Dt' (Int,Int,Int)
                deriving (Eq,Show)

d5 = Dt' (2,4,2018)            

--newtype Color = Red Int | Green Int | Blue Int

-- A (silly example of a) custom class

class (Ord a) => MyEq a where
    (===) :: a -> a -> Bool
    
    x === y  = (x <= y) && (y <= x)

-- using default implementation
instance MyEq Day
    
b101 = today === next today
b102 = today === today

-- the constraint is not satisfied
{-
instance MyEq Date where
	x === y = x == y
-}
--

data Nat = Z | S Nat
        deriving (Eq,Ord,Show)

n1 = S (S (S Z))
n2 = S (S Z)

b4 = n2 < n1

int2nat :: (Integral a) => a -> Nat
int2nat n | n < 0       = error "negative value"
          | n == 0      = Z
          | otherwise   = S (int2nat (n - 1))

nadd :: Nat -> Nat -> Nat
nadd m Z  = m
nadd m (S n) = S (nadd n m)

-- Fibonacci, etc.

n3 = nadd n1 n2

nat2int :: (Integral a) => Nat -> a
nat2int Z = 0
nat2int (S n) = (nat2int n) + 1

foldn :: (a -> a) -> a -> Nat -> a
foldn f x Z = x
foldn f x (S n) = f (foldn f x n)

nat2int' :: Nat -> Int
nat2int' = foldn (+1) 0

nadd' = foldn (\f -> S . f) id
-- What is the type of nadd'?
-- cf. the well-know lambda term for Church numeral addition
nadd'' = foldn S

npred = snd . foldn (\(n,x) -> (S n, n)) (Z,Z)

nmlt :: Nat -> Nat -> Nat
nmlt m =  (\(_,_,z) -> z) . (foldn (\(l,k,y) -> (l, S k, nadd' y l)) (m,Z,Z))

-- k is not needed
nmlt' m =  snd . (foldn (\(l, y) -> (l, nadd' y l)) (m,Z))

-- direct iteration pure lambda style
nmlt'' n m = foldn (foldn S m) Z n


nfac = snd . foldn (\(n,x) -> (S n, nmlt (S n) x )) (Z, S Z)


instance Num Nat where
    n + m = nadd n m
    n * m = nmlt n m
    negate _ = Z
    signum Z = Z
    signum (S _) = S Z
    abs = id
    fromInteger = int2nat

n5 = (n1 * n2) + (nfac n3)

-- instance Integral Nat where ...
--

data Tree a = Nil | Node (Tree a) a (Tree a)
            deriving (Eq,Show)


myTree = Node (Node (Node (Node (Node (Node Nil 2 Nil) 5 (Node Nil 6 Nil)) 7 Nil ) 10 (Node (Node Nil 10  Nil) 11 Nil))
        13  (Node Nil 15 (Node Nil 16 Nil)))
                17  (Node Nil 23 (Node Nil 25 Nil))

myTree1 = Node (Node Nil 10 Nil) 20 (Node (Node Nil 5 Nil) 30 (Node Nil 40 Nil))

-- Maybe a = Nothing | Just a

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- maybe :: b -> (a -> b) -> Maybe a -> b

sqhead :: [Int] -> Int
sqhead ns = maybe 0 (^2) (safehead ns)

--

root :: Tree a -> Maybe a            
root Nil = Nothing
root (Node _ x _) = Just x

bstmax :: (Ord a) => Tree a -> Maybe a
bstmax Nil = Nothing
bstmax (Node l x r) = Just (maybe x id (bstmax r))

bstmin :: (Ord a) => Tree a -> Maybe a
bstmin Nil = Nothing
bstmin (Node l x r) = Just (maybe x id (bstmin l))

isBST :: (Ord a) => Tree a -> Bool
isBST Nil = True
isBST (Node l x r) = isBST l && maybe True (<= x) (bstmax l) &&
                    isBST r && maybe True (>= x) (bstmin r)

insBST :: (Ord a) => a -> Tree a -> Tree a
insBST x Nil = Node Nil x Nil
insBST x (Node l y r) = case compare x y of
                     LT -> Node (insBST x l) y r
                     EQ -> Node l y r
                     GT -> Node l y (insBST x r)

list2BST :: Ord a => [a] -> Tree a
list2BST = foldr insBST Nil

flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x Nil = False
occurs x (Node l y r) = case compare x y of
                        LT -> occurs x l
                        EQ -> True
                        GT -> occurs x r
                      
--

foldt :: (b -> a -> b -> b) -> b -> Tree a -> b
foldt f x Nil = x
foldt f x (Node l y r) = f (foldt f x l) y (foldt f x r)

mapt :: (a -> b) -> Tree a -> Tree b
mapt g = foldt (\l x r -> Node l (g x) r) Nil

----------------

height :: Tree a -> Int
height = foldt (\x _ y -> 1 + max x y) 0

-- balanced in AVL style
isBlncd :: Tree a -> Bool
isBlncd Nil = True
isBlncd (Node l _ r) = isBlncd l && isBlncd r && abs(height l - height r) <= 1

{-
rotr :: Tree a -> Tree a
rotr (Node (Node l x r) y r') = Node l x (Node r y r')

rotl :: Tree a -> Tree a
rotl (Node l' x (Node l y r)) = Node (Node l' x l) y r

myTree2 = Node (Node (Node (Node (Node Nil 0 Nil) 1 Nil) 2 Nil ) 3 Nil) 4
        (Node Nil 5 (Node Nil 6 (Node Nil 7 (Node Nil 8 Nil))))

myTree3 =  Node (Node (Node (Node Nil 0 Nil) 1 Nil ) 2 Nil) 3
    (Node (Node (Node Nil 7 Nil) 6 Nil) 4 (Node (Node Nil 8 Nil) 5 (Node Nil 9 (Node Nil 10 Nil))))

myTree4 = Node (Node (Node (Node Nil 1 Nil) 2 Nil) 3 (Node Nil 4 (Node Nil 5 Nil))) 6 Nil
-}
--

data Prop = Cst Bool
           | Vr Char
           | Not Prop
           | And Prop Prop
           | Imp Prop Prop
           | Or Prop Prop
           | Eqv Prop Prop
          deriving (Eq,Read)

p = Vr 'p'
q = Vr 'q'
r = Vr 'r'
s = Vr 's'
t = Vr 't'
myProp = Imp (Not p) (Or q r)
cases = Imp (Imp p r) (Imp (Imp q r) (Imp (Or p q) r))
pierce = Imp (Imp (Imp p q) p) p
tnd = Or (Not p) p

instance Show Prop where
    show (Cst b) = if b then "T" else "F"
    show (Vr c) = [c]
    show (Not p) = "~" ++ show p
    show (And p q) = "(" ++ show p ++ " /\\ " ++ show q ++ ")"
    show (Or p q) = "(" ++ show p ++ " \\/ " ++ show q ++ ")"
    show (Imp p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
    show (Eqv p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"


evalc :: Prop -> Bool
evalc (Cst b) = b
evalc (Not p) =  not (evalc p)
evalc (And p q) = (evalc p) && (evalc q)
evalc (Imp p q) = (evalc p) <= (evalc q)
evalc (Or p q) = (evalc p) || (evalc q)
evalc (Eqv p q) = (evalc p) == (evalc q)

vars :: Prop -> [Char]
vars (Cst b) = []
vars (Vr v) = [v]
vars (Not p) = vars p
vars (And p q) = vars p ++ [v | v <- vars q, null (filter (==v) (vars p))]
vars (Imp p q) = vars p ++ [v | v <- vars q, null (filter (==v) (vars p))]
vars (Or p q) = vars p ++ [v | v <- vars q, null (filter (==v) (vars p))]
vars (Eqv p q) = vars p ++ [v | v <- vars q, null (filter (==v) (vars p))]

const4var :: Bool -> Prop -> Char -> Prop
const4var _ (Cst b) _ = Cst b
const4var b (Vr v) u = if u == v then Cst b else Vr v
const4var b (Not p) u = Not (const4var b p u)
const4var b (And p q) u = And (const4var b p u) (const4var b q u)
const4var b (Imp p q) u = Imp (const4var b p u) (const4var b q u)
const4var b (Or p q) u = Or (const4var b p u) (const4var b q u)
const4var b (Eqv p q) u = Eqv (const4var b p u) (const4var b q u)

elimvar :: Prop -> Char -> Prop
elimvar p v = And (const4var True p v) (const4var False p v)

clsr :: Prop -> Prop
clsr p = foldl elimvar p (vars p)

isTaut :: Prop -> Bool
isTaut = evalc . clsr

isSat :: Prop -> Bool
isSat = not . isTaut . Not

--

type Subst = Assoc Char Bool

evalp :: Subst -> Prop -> Bool
evalp _ (Cst b) = b
evalp s (Vr v) = find s v
evalp s (Not p) =  not (evalp s p)
evalp s (And p q) = (evalp s p) && (evalp s q)
evalp s (Imp p q) = (evalp s p) <= (evalp s q)
evalp s (Or p q) = (evalp s p) || (evalp s q)
evalp s (Eqv p q) = (evalp s p) == (evalp s q)

substs :: [Char] -> [Subst]
substs [] = []
substs [c] = [[(c,True)], [(c,False)]]
substs (c:cs) = map ((c,True) :) ss ++ map ((c,False) :) ss
                where ss = substs cs

tableau :: Prop -> [Bool]
tableau p = map (\s -> evalp s p) (substs (vars p))

isTaut' :: Prop -> Bool
isTaut' = and . tableau

isSat' :: Prop -> Bool
isSat' = or . tableau

--

{-- Finding an interpolant for a tautology p -> q --}

-- variables unique for q
unqvar :: Prop -> Prop -> [Char]
unqvar p q = filter (\x -> not (elem x vs)) (vars q)
            where vs = vars p

intpl :: Prop -> Prop -> Prop
intpl p q = foldl elimvar q (unqvar p q)

testIntpl :: Prop -> Prop -> Bool
testIntpl p q = if isTaut (Imp p q) then
                   (isTaut (Imp p i)) && (isTaut (Imp i q))
                else True
            where i = intpl p q

prop1 = And (Not q) (And r s)
prop2 = Imp s (Imp q t)
prop3 = Imp prop1 prop2

{-- Another representation of propositions. --}

data Cnt = A | O | I | E
    deriving (Eq,Show)

data Prp = C Bool | V Char | N Prp |
            Bin Cnt Prp Prp
    deriving (Eq,Show)


{-- Unification --}
{--
dis2 :: Prop -> Prop -> [Prop]
dis2 (Cst x) (Cst y) = if x == y then [] else [Cst x, Cst y]
dis2 (Vr x) (Vr y) = if x == y then [] else [Vr x, Vr y]
dis2 (Not p) (Not q) = if p == q then [] else [p, q]
dis2 (And p p') (And q q') = if p == q then
                                if p' == q' 
                          else [p, q]


-- disagreement set
--disagr = [Prop] -> [Prop]

--}
