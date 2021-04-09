-- auxiliary function
upart' _ 0 = [[]]
upart' 0 _ = [[]]
upart' 1 _ = [[1]]
upart' n 1 = [take n [1,1..]]
upart' target max_elem = concat [map (x:) (upart' (target - x) x) | x <- [lim',(lim'-1)..1]]
    where
    lim' = min target max_elem

upart n = upart' n n
-- *Main> upart 5
-- [[5],[4,1],[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]]
-- *Main> upart 7
-- [[7],[6,1],[5,2],[5,1,1],[4,3],[4,2,1],[4,1,1,1],[3,3,1],[3,2,2],[3,2,1,1],[3,1,1,1,1],[2,2,2,1],[2,2,1,1,1],[2,1,1,1,1,1],[1,1,1,1,1,1,1]]

main = print "ok"
