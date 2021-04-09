clean_duplicates' [] _ = []
clean_duplicates' (x:xs) seen 
    | elem x seen = clean_duplicates' xs seen
    | otherwise = x : (clean_duplicates' xs (x:seen))
                        
clean_duplicates x = clean_duplicates' x []
-- *Main> clean_duplicates [1, 2, 3, 2, 4, 5, 8, 7, 9, 7, 8]
-- [1,2,3,4,5,8,7,9]

main = print "ok"
