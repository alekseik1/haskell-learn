f :: Num a => a -> a -> a
f x y = x*x + y*y
g :: Num a => a -> a
g = f 3
-- Equal to
g_ = \y -> 3*3 + y*y
main = print (g_ 3)
