import Data.Char
import System.IO

upside_down :: IO()
upside_down = do eof <- isEOF
                 if not eof then
                     do l <- getLine
                        putStr $ map (\sym -> if isLower sym then (toUpper sym) else (toLower sym)) l
                        upside_down
                 else
                     return ()

-- $ echo "I dOn'T FeeL sO gOod" | ./44
-- i DoN't fEEl So GoOD

main = upside_down
