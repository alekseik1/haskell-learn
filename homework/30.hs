-- map
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' xs = (map fst xs, map snd xs)

-- foldr
unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\x acc -> (fst x:fst acc, snd x:snd acc)) ([],[])


-- Copy-paste this part
assert_equal expected actual = 
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test_map1 <- assert_equal (unzip' [(1, "a"), (2, "b")]) ([1, 2], ["a", "b"])
    test_map2 <- assert_equal (unzip' (zip [1..100] [2,4..200])) ([1..100], [2,4..200])
    test_foldr1 <- assert_equal (unzip'' [(1, "a"), (2, "b")]) ([1, 2], ["a", "b"])
    test_foldr2 <- assert_equal (unzip'' (zip [1..100] [2,4..200])) ([1..100], [2,4..200])
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
