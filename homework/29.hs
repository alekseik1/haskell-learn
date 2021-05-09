-- iterate
rotts :: [a] -> [[a]]
rotts [a] = [[a]]
rotts list = take (length list) $ iterate (\(x:xs) -> xs ++ [x]) list

-- scanl
rotts' :: [a] -> [[a]]
rotts' [a] = [[a]]
rotts' list = scanl (\(x:xs) _ -> xs ++ [x]) list [1..length list - 1]


-- Copy-paste this part
assert_equal expected actual = 
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test1 <- assert_equal (rotts [1, 2, 3]) [[1,2,3],[2,3,1],[3,1,2]]
    test2 <- assert_equal (rotts [1, 5, 2, 4]) [[1,5,2,4],[5,2,4,1],[2,4,1,5],[4,1,5,2]]
    test3 <- assert_equal (rotts' [1, 2, 3]) [[1,2,3],[2,3,1],[3,1,2]]
    test4 <- assert_equal (rotts' [1, 5, 2, 4]) [[1,5,2,4],[5,2,4,1],[2,4,1,5],[4,1,5,2]]
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
