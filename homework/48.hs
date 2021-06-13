import System.Random

qsplit at xs = (le, x, gt)
    where (left, (x:right)) = splitAt at xs
          rest = left ++ right
          le = filter (<= x) rest
          gt = filter (> x) rest

kth_random :: (Ord a, RandomGen gen) => Int -> [a] -> gen -> a
kth_random k xs g
    | pos == k = x
    | pos > k = kth_random k le next_gen
    | pos < k = kth_random (k - pos) gt next_gen
    where (at, next_gen) = randomR (0, length xs - 1) g
          pos = length le + 1
          (le, x, gt) = qsplit at xs

kth :: Ord a => Int -> [a] -> IO a
kth k xs = kth_random k xs <$> getStdGen
