triples = [(x, y, z) | z <- [1..], x <- [1..z], y <- [1..z], x^2 + y^2 == z^2, x <= y]

main = print "ok"
