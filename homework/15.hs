nrem n = foldr aggr [] . zip [1..]
    where aggr (i,x) acc = if (i `mod` n) == 0 then acc else x:acc
main = print "ok"
