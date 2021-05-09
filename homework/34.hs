compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = (x, 1 + length (takeWhile (==x) xs)) : compress (dropWhile (==x) xs)


decompress :: Eq a => [(a, Int)] -> [a]
decompress l = concat $ map (\(char, nums) -> replicate nums char) l


-- Copy-paste this part
assert_equal expected actual = 
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test_compress_1 <- assert_equal (compress ["a", "a", "b", "c"]) [("a", 2), ("b", 1), ("c", 1)]
    test_compress_2 <- assert_equal (compress [2, 5, 5, 1, 1, 1]) [(2, 1), (5, 2), (1, 3)]
    test_decompress_1 <- assert_equal (decompress [("a", 2), ("b", 1)]) ["a", "a", "b"]
    test_decompress_2 <- assert_equal (decompress [(2, 1), (5, 2), (1, 3)]) [2, 5, 5, 1, 1, 1]
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
