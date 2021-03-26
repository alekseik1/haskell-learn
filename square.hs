square :: Num a => a -> a
square x = x^2

-- parenthesis means "prefix notation"
square' x = (^) x 2
square'' x = (^2) x
-- square = lambda x: x^2 <==> square = ^2
square''' = (^2)
