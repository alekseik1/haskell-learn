import System.Environment


-- askUser :: IO [Integer]
askUser = do
  -- putStrLn "Enter a list of numbers (separated by comma):"
  input <- getArgs
  -- input <- getLine
  return $ map read input

main :: IO ()
main = do
  list <- askUser
  print $ sum list
