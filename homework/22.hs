-- (A)B <- вот так любую правильную последовательность можно представить
pars :: Int -> [[Char]]
pars 0 = [""]
pars n = [ "(" ++ x ++ ")" ++ y
         | m <- [0..n-1], x <- pars m, y <- pars (n-m-1)]

main = print "ok"
