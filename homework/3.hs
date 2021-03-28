-- a) конъюнкция
conj' :: Bool -> Bool -> Bool
conj' x True = x
conj' _ False = False
-- b) импликация
impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' x y = True
-- c) исключающее или
xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' x False = x
xor' _ True = True
-- d) функция большинства maj_3
maj_3 :: Bool -> Bool -> Bool -> Bool
maj_3 True True _ = True
maj_3 False False _ = False
maj_3 True False x = x
maj_3 False True x = x

main = print "ok"

