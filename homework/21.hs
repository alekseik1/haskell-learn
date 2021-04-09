factors n = [x | x <- [2..(n`div` 2)], mod n x == 0]
is_prime = null . factors
split_into_primes n = [(x, n - x) | x <- [2..(n `div` 2)], is_prime x, is_prime (n - x)]
-- *Main> split_into_primes 13
-- [(2,11)]
-- *Main> split_into_primes 11
-- []

main = print "ok"
