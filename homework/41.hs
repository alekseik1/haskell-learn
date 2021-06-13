data Tm = N Int | V Char | Add Tm Tm | Sbt Tm Tm | Mlt Tm Tm


-- a)
instance Num Tm where
   (+) a b = Add a b
   (*) a b = Mlt a b
   (-) a b = Sbt a b
   fromInteger = N . fromIntegral
   abs x = Mlt x $ signum x
   signum (N n) = N $ signum n

-- b)
instance Show Tm where
   show (N n) = show n
   show (V v) = [v]
   show (Add tm1 tm2) = show tm1 ++ " + " ++ show tm2
   show (Sbt tm1 tm2) = show tm1 ++ " - " ++ show tm2

   show (Mlt (N n1) (N n2)) = show n1 ++ " * " ++ show n2
   show (Mlt (N n) (V v)) = show n ++ " * " ++ [(show v)!!1]
   show (Mlt (V v) (N n)) = [(show v)!!1] ++ " * " ++ show n
   show (Mlt (V v1) (V v2)) = [(show v1)!!1] ++ [(show v2)!!1]
   show (Mlt (N n) tm) = show n ++ " * (" ++ show tm ++ ")"
   show (Mlt tm (N n)) = "(" ++ show tm ++ ") * " ++ show n
   show (Mlt (V v) tm) = show v ++ " * (" ++ show tm ++ ")"
   show (Mlt tm (V v)) = "(" ++ show tm ++ ") * " ++ show v
   show (Mlt tm1 tm2) = "(" ++ show tm1 ++ ") * (" ++ show tm2 ++ ")"

-- *Main> (5 + V 'x') * 5
-- (5 + x) * 5
-- *Main> (5 + V 'x') * (2 + V 'x')
-- (5 + x) * (2 + x)
-- *Main> 2 * (5 + V 'x')
-- 2 * (5 + x)
-- *Main> (2 + V 'y') * (5 + V 'x')
-- (2 + y) * (5 + x)
-- *Main> (N 2) * (V 'x')
-- 2 * x
-- *Main> (V 'y') * (V 'x')
-- yx
-- *Main> (V 'z') * 5
-- z * 5

main = print "ok"
