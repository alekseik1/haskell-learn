module Nats where

import Data.Ratio

data Nat = Z | S Nat deriving (Eq, Show, Ord)


instance Enum Nat where
   succ = S
   pred Z = Z
   pred (S n) = n
   toEnum   = fromIntegral
   fromEnum = foldn succ 0
   enumFrom n = map toEnum [(fromEnum n)..]

instance Num Nat where
   (+) = foldn succ
   (*) = \m -> foldn (+m) Z
   (-) = foldn pred
   abs = id
   signum Z = Z
   signum n = (S Z)
   fromInteger n | n < 0     = error "no negative naturals"
                 | n == 0    = Z
                 | otherwise = S (fromInteger (n-1))

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Z = c
foldn h c (S n) = h (foldn h c n)

instance Real Nat where toRational x = toInteger x % 1

instance Integral Nat where
    quotRem n d  | d > n     = (Z,n)
                 | otherwise = (S q, r) where
                    (q,r) = quotRem (n-d) d
    mod n d | d > n = n
            | otherwise = mod (n-d) d

    div n d | d > n = Z
            | otherwise = mod (n-d) d

    toInteger = foldn succ 0

-- *Nats> div (fromInteger 5) (fromInteger 2)
-- 2
-- *Nats> div (fromInteger 5) (fromInteger 6)
-- 0
-- *Nats> div (fromInteger 5) (fromInteger 3)
-- 1
-- *Nats> div (fromInteger 5) (fromInteger 4)
-- 1
-- *Nats> mod (fromInteger 5) (fromInteger 3)
-- 2
-- *Nats> mod (fromInteger 5) (fromInteger 2)
-- 1
-- *Nats> mod (fromInteger 5) (fromInteger 1)
-- 0
-- *Nats> mod (fromInteger 5) (fromInteger 4)
-- 1
-- *Nats> mod (fromInteger 23) (fromInteger 12)
-- 11

main = print "ok"
