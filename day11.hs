import Data.List
import Control.Arrow ((&&&),(***))

main = let day = "11" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = (`div`2) . sum . distances
       . (\rows -> [(x,y) | (y,row)<-zip [0..] rows, (x,c)<-zip [0..] row, c=='#'])
       . expand . transpose
       . expand . lines
  where expand = foldr (\x xs -> if all (=='.') x then x:x:xs else x:xs) []

solve2 = (`div`2) . sum . distances
       . (\(rows,_) -> [(x,y) | row<-rows, (x,(y,c))<-row, c=='#'])
       . expandedIndex . (transpose *** transpose)
       . expandedIndex . (id &&& id) . lines
  where expandedIndex :: ([[a]],[[Char]]) -> ([[(Int,a)]],[[Char]])
        expandedIndex (ys,xs) = (fst $ foldr index ([],0) (zip ys xs), xs)
        index (y,x) (iys,i) = (zip [i,i..] y : iys,
                               if all (=='.') x then i+1000000 else i+1)

distances :: [(Int,Int)] -> [Int]
distances cs = (\(x,y) (x',y') -> abs (x-x') + abs (y-y')) <$> cs <*> cs
