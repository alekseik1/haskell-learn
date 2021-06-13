import System.Random
import Data.Array.IO
import Control.Monad

popFromList :: Int -> [a] -> [a]
popFromList i xs
    | length xs > i = (\(ys, (z:zs)) -> (ys ++ zs)) $ (splitAt i xs)
    | otherwise = error "Not enough elements"

suffle' :: RandomGen g => [a] -> g -> [a]
suffle' [] _ = []
suffle' xs gen = let (idx, newGen) = randomR (0, length xs - 1) gen
                     x = xs !! idx
                     rest = popFromList idx xs
                     shuffledRest = suffle' rest newGen in x:shuffledRest

shuffle :: [a] -> IO([a])
shuffle [] = return []
shuffle xs = suffle' xs <$> getStdGen


-- Another way
-- Random at each step
shuffle'' :: [a] -> IO [a]
shuffle'' xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
