import Text.Read
import Data.Maybe

read_while_number :: IO()
read_while_number = do l <- getLine
                       let r = readMaybe l :: Maybe Double
                       if isNothing r then
                          return ()
                       else
                          read_while_number

main = read_while_number
