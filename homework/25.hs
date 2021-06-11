foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f xs = foldr f (last xs) (init xs)

-- Copy-paste this part
assert_equal expected actual =
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test_1 <- assert_equal (foldr1' (+) [1, 2, 3, 4]) 10
    test_2 <- assert_equal (foldr1' (*) [1, 2, 3, 4]) 24
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
