zapp :: [a -> b] -> [a] -> [b]
zapp = curry ((map (uncurry ($))) . (uncurry zip))

-- Copy-paste this part
assert_equal expected actual =
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test1 <- assert_equal (zapp [(*2), (^2)] [5, 9]) [10, 81]
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
