import Data.List
import Control.Arrow ((&&&),(***))

main = let day = "11" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = (`div`2) . sum . distances . coords
       . expand . transpose . expand . lines
  where
    coords rows = [(x,y) | (y,row)<-zip [0..] rows, (x,'#')<-zip [0..] row]
    expand = foldr (\x xs -> if all (=='.') x then x:x:xs else x:xs) []

solve2 = (`div`2) . sum . distances . coords'
       . indexExpand . (transpose *** transpose) . indexExpand . (id &&& id) . lines
  where
    coords' (_,rows) = [(x,y) | row<-rows, (x,(y,'#'))<-row]
    indexExpand (xs,zs) = (xs, snd $ foldr index (0,[]) (zip xs zs))
    index (x,z) (i,izs) = (if all (=='.') x then i+1000000 else i+1,
                           zip [i,i..] z : izs)

distances :: [(Int,Int)] -> [Int]
distances cs = (\(x,y) (x',y') -> abs (x-x') + abs (y-y')) <$> cs <*> cs
