import System.IO
import Data.Char
import Control.Exception

getInt :: IO Int
getInt = do l <- getLine
            return (read l)

getInts :: Int -> IO [Int]
getInts n | n < 0       = error "Negative argument number!"
          | n == 0      = return []
          | otherwise   = do x <- getInt
                             xs <- getInts (n-1)
                             return (x:xs)

whichOperator :: String -> Maybe ([Int] -> Int)
whichOperator "+" = Just sum
whichOperator "*" = Just (foldr (*) 1)
whichOperator _ = Nothing

getOperation ::  IO ([Int] -> Int)
getOperation = do putStrLn "Which operation?"
                  l <- getLine
                  let op = whichOperator l
                  case op of Just f -> return f
                             Nothing -> getOperation

main = do 
putStrLn "How many numbers? "
n <- getInt
xs <- getInts n
op <- getOperation
putStrLn "The total is "
putStrLn (show (op xs))

