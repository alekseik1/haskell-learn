segs :: [a] -> [[a]]
segs [] = []
segs [x] = [[x]]
segs list = [drop start (take end list) 
    | end <- [1..length list],
      start <- [0..length list], 
      end > start]

main = print "ok"
