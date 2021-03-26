sum_n :: Int -> Int
sum_n 0 = 0
sum_n x = x + sum_n (x-1)
triangle_numbers = [sum_n x | x <- [1..]]

main = print ("ok")
