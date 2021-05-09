lmax' :: Ord a => [a] -> a
lmax' = foldr1 (\x acc -> if x >= acc then x else acc)

assert_equal expected actual = 
    if expected == actual
    then Just True
    else Nothing

tests = do
    test1 <- assert_equal (lmax' [1, 2, 1]) 2
    test2 <- assert_equal (lmax' [1, 2, 9, 1]) 9
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
