dupl :: [a] -> [a]
dupl [] = []
dupl (x:xs) = [x, x] ++ dupl xs
main = print "ok"
