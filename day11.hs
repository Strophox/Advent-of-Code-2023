import Data.List
import Control.Arrow ((&&&),(***))

main = let day = "11" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = pairDistances . index . expand . transpose . expand . lines
  where expand = foldr (\a b -> if all (=='.') a then a:a:b else a:b) []
        index rows = [(x,y) | (y,row)<-zip [0..] rows,
                              (x,chr)<-zip [0..] row, chr=='#']


solve2 = pairDistances . (\(rows,_) -> [(u,v) | row<-rows, (u,(v,c))<-row, c=='#'])
       . expandedIndex . (transpose *** transpose)
       . expandedIndex . (id &&& id) . lines
  where expandedIndex (xs,os) = (fst $ foldr build ([],0) (zip xs os), os)
        build (x,o) (xs',i) = ((zip [i,i..] x):xs',
                               if all (=='.') o then i+1000000 else i+1)

pairDistances :: [(Int,Int)] -> Int
pairDistances cs = sum allDistances `div` 2
  where allDistances = (\(x,y) (x',y') -> abs (x-x') + abs (y-y')) <$> cs <*> cs
