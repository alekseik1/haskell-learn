prefixes :: String -> [String]
prefixes (x:xs) = scanl (\acc x -> acc ++ [x]) [x] xs

-- Copy-paste this part
assert_equal expected actual = 
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test1 <- assert_equal (prefixes "hello") ["h", "he", "hel", "hell", "hello"]
    test2 <- assert_equal (prefixes "haskell") ["h","ha","has","hask","haske","haskel","haskell"]
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
