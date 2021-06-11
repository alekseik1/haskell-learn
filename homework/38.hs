data Tree a = Nil | Node (Tree a) a (Tree a)
            deriving (Eq,Show)

foldt :: (b -> a -> b -> b) -> b -> Tree a -> b
foldt f x Nil = x
foldt f x (Node l y r) = f (foldt f x l) y (foldt f x r)

mapt :: (a -> b) -> Tree a -> Tree b
mapt g = foldt (\l x r -> Node l (g x) r) Nil

-- insert AVL
insAVL :: (Ord a) => a -> Tree a -> Tree a
insAVL a = fst . (insAVLHelper a)

-- helpers
insAVLHelper :: (Ord a) => a -> Tree a -> (Tree a, Int)
insAVLHelper x Nil = (Node Nil x Nil, 1)
-- rotation ref: https://ru.wikipedia.org/wiki/АВЛ-дерево
insAVLHelper x (Node l y r)
    | x <= y = let (newL, left_depth) = insAVLHelper x l
                   right_depth = treeDepth r
                   newTree = Node newL y r in
                   if left_depth > right_depth + 1 then
                       (rotateRight newTree, left_depth)
                   else
                       (newTree, left_depth + 1)
    | x > y = let (newR, right_depth) = insAVLHelper x r
                  left_depth = treeDepth l
                  newTree = Node l y newR in
                  if right_depth > left_depth + 1 then
                      (rotateLeft newTree, right_depth)
                  else
                      (newTree, right_depth + 1)

treeDepth :: Tree a -> Int
treeDepth = foldt (\x _ z -> (max x z) + 1) 0

rotateRight :: Tree a -> Tree a
rotateRight (Node (Node ll lx lr) x r) = Node ll lx (Node lr x r)

rotateLeft :: Tree a -> Tree a
rotateLeft (Node l x (Node rl rx rr)) = Node (Node l x rl) rx rr

myTree = Node (Node (Node (Node (Node (Node Nil 2 Nil) 5 (Node Nil 6 Nil)) 7 Nil ) 10 (Node (Node Nil 10  Nil) 11 Nil))
        13  (Node Nil 15 (Node Nil 16 Nil)))
                17  (Node Nil 23 (Node Nil 25 Nil))

-- Copy-paste this part
assert_equal expected actual =
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test_ins1 <- assert_equal (insAVL 10 myTree) (Node (Node (Node (Node (Node Nil 2 Nil) 5 (Node Nil 6 Nil)) 7 (Node Nil 10 Nil)) 10 (Node (Node Nil 10 Nil) 11 Nil)) 13 (Node (Node Nil 15 (Node Nil 16 Nil)) 17 (Node Nil 23 (Node Nil 25 Nil))))
    sum_2 <- assert_equal (insAVL 5 myTree) (Node (Node (Node Nil 2 (Node Nil 5 Nil)) 5 (Node (Node Nil 6 Nil) 7 Nil)) 10 (Node (Node (Node (Node Nil 10 Nil) 11 Nil) 13 (Node Nil 15 (Node Nil 16 Nil))) 17 (Node Nil 23 (Node Nil 25 Nil))))
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
