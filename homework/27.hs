scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f init list = reverse $ foldl (\(acc:acc_xs) x -> (f acc x):acc:acc_xs) [init] list

-- Copy-paste this part
assert_equal expected actual = 
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test1 <- assert_equal (scanl' (/) 64 [4, 2, 4]) [64.0,16.0,8.0,2.0]
    test2 <- assert_equal (scanl' (/) 3 []) [3.0]
    test3 <- assert_equal (scanl' (\x y -> 2*x + y) 4 [1,2,3]) [4,9,20,43]
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
