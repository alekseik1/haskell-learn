f1 :: Int -> Int
f1 n = if n < 0 then
                    (-1)
                else 
                    if n == 0 then
                        2016
                    else
                        2018
f2 :: Int -> Int
f2 = \n -> if n < 0 then 3 else if n > 0 then 2 else (-6)

f3 :: Int -> Int
f3 n = case n of
            1           -> 5
            2           -> 4
            3           -> 3
            otherwise -> 2018

f4 :: (Int,Char) -> Int
f4 x = case x of 
            (4,_)   -> 18
            (_,'u') -> 19
--            (4,'v') -> (-1)
            (5,'A')   -> 12

--            

f5 :: [Int] -> Int
f5 [] = 0
f5 [_,2,_] = (-2)
f5 [1,3,7] = (-1)
f5 (x:xs) = x * sum xs

myOr :: Bool -> Bool -> Bool
myOr True _ = True
myOr False x = x

myOr1 False False = False
myOr1 _ _ = True

-- Each variable occurs at most once!
-- myOr2 x x = x
-- myOr2 _ _ = True

type Triple a = (a,a,a)
type R3 = Triple Float

vsum :: R3 -> R3 -> R3
vsum (x,y,z) (u,v,w) = (x + u, y + v, z + w)

vsum1 :: R3 -> R3 -> R3
vsum1 p q = (pr 1 p + pr 1 q, pr 2 p + pr 2 q, pr 3 p + pr 3 q)
   
pr :: Int -> Triple a -> a
pr 1 (x,_,_)  = x 
pr 2 (_,y,_) = y
pr 3 (_,_,z) = z

err = error "Too short list"

-- Safe third
sthird :: [a] -> a
sthird (_:_:x:_) = x
sthird _ = err

--
sthird1 :: [a] -> a
sthird1 xs | length xs >= 3     = xs !! 2
           | otherwise          = err

sgn :: Int -> Int
sgn n | n < 0   = (-1)
      | n == 0  = 0
      | n > 0   = 1

shead :: [a] ->  a
shead xs | null xs      = err
         | otherwise    = head xs

stail :: [a] -> [a]        
stail [] = err
stail (_:xs) = xs

-- Bisecting an even-lengthed list
halve :: [a] -> ([a],[a])
halve ns = if  (+) l l == (length ns) then (take l ns, drop l ns) else (ns,[])
      where l = (length ns) `div` 2

-- Note that where goes AFTER the whole guarded thing
halve2 ns | l * 2 == (length ns)    = (take l ns, drop l ns)
          | otherwise               = (ns,[])
            where l = (length ns) `div` 2

-- "Near"-bisecting 
ihalve3 ns = let l = (length ns) `div` 2 in (take l ns, drop l ns)                                           

--
cnst2 :: a -> Int
cnst2 = \_ -> 2

myComp :: (b -> c) -> (a -> b) -> (a -> c)
myComp g f x = g (f x)

twice :: (a -> a) -> (a -> a)
twice f = f . f

myThird :: [a] -> a
myThird = head . tail . tail


-- "Initializer"
intzr :: [String] -> String
intzr [] = []
intzr ss = (concat . (map (\s -> (head s : ". "))) . init) ss ++ last ss


--


