data Nat = Z | S Nat
        deriving (Eq,Ord,Show)

int2nat :: (Integral a) => a -> Nat
int2nat n | n < 0       = error "negative value"
          | n == 0      = Z
          | otherwise   = S (int2nat (n - 1))

foldn :: (a -> a) -> a -> Nat -> a
foldn f x Z = x
foldn f x (S n) = f (foldn f x n)

nadd :: Nat -> Nat -> Nat
nadd m Z  = m
nadd m (S n) = S (nadd n m)

-- recursion
fib :: Nat -> Nat
fib Z = Z
fib (S Z) = (S Z)
fib (S (S k)) = nadd (fib k) (fib(S k))

-- foldn
fib' :: Nat -> Nat
fib' = fst . foldn f (Z, S Z) where f (x,y) = (y, nadd x y)

-- Copy-paste this part
assert_equal expected actual = 
    if expected == actual
    then Just True
    else Nothing

-- Adjust tests
tests = do
    test_fib_a_1 <- assert_equal (fib (int2nat 5)) $ int2nat 5
    test_fib_a_2 <- assert_equal (fib (int2nat 6)) $ int2nat 8
    test_fib_a_3 <- assert_equal (fib (int2nat 0)) Z
    test_fib_b_1 <- assert_equal (fib' (int2nat 0)) Z
    test_fib_b_2 <- assert_equal (fib' (int2nat 5)) $ int2nat 5
    test_fib_a_3 <- assert_equal (fib (int2nat 6)) $ int2nat 8
    test_fib_a_4 <- assert_equal (fib (int2nat 7)) $ int2nat 13
    Just True

main = if tests == (Just True) then print "ok" else error "tests fail"
