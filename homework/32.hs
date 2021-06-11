-- g :: A -> B -> D
-- h :: D -> C
-- f :: A -> B -> C
-- f x y = h (g x y)
-- uncurry g :: (a, b) -> d
-- h . (uncurry g) :: (a, b) -> c
-- f = curry $ h . (uncurry g)

main = print "ok"
