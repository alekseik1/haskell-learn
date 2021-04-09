kth_order k (x:xs)
    | k == m = x
    | k < m = kth_order k lesser
    | k > m = kth_order (k - m) greater
    where
    lesser = [a | a <- xs, a < x]
    greater = [a | a <- xs, a > x]
    m = length lesser + 1

main = print "ok"
