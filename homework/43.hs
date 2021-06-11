waitAndMax :: String -> IO String
waitAndMax p = do l <- getLine
                  if l == "END" then
                    return p
                  else
                    waitAndMax $ max l p

main = (waitAndMax "") >>= putStrLn
