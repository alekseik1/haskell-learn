curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = (\x y -> f (x, y))


uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = (\(x, y) -> f x y)


-- Copy-paste this part
assert_equal expected actual = 
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test1 <- assert_equal ((curry' (\ (x,y) -> 2*x+y)) 2 3) 7
    test1 <- assert_equal ((uncurry' (*)) (2, 3)) 6
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
