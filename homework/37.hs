data Tree a = Nil | Node (Tree a) a (Tree a)
            deriving (Eq,Show)

foldt :: (b -> a -> b -> b) -> b -> Tree a -> b
foldt f x Nil = x
foldt f x (Node l y r) = f (foldt f x l) y (foldt f x r)

mapt :: (a -> b) -> Tree a -> Tree b
mapt g = foldt (\l x r -> Node l (g x) r) Nil

-- sum
-- sumTree :: Tree Int -> Int
sumTree t = foldt sumNodes' 0 t

-- sumNodes' :: Int -> Int -> Int -> Int
sumNodes' n1 n2 n3 = n1 + n2 + n3

myTree = Node (Node (Node (Node (Node (Node Nil 2 Nil) 5 (Node Nil 6 Nil)) 7 Nil ) 10 (Node (Node Nil 10  Nil) 11 Nil))
        13  (Node Nil 15 (Node Nil 16 Nil)))
                17  (Node Nil 23 (Node Nil 25 Nil))

myTree1 = Node (Node Nil 10 Nil) 20 (Node (Node Nil 5 Nil) 30 (Node Nil 40 Nil))

-- Copy-paste this part
assert_equal expected actual =
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    sum_1 <- assert_equal (sumTree myTree) 160
    sum_2 <- assert_equal (sumTree myTree1) 105
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
