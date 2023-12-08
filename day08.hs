import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map,fromList,toList,(!))

main = let day = "08" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = walkFrom "AAA" (=="ZZZ") . parse

solve2 = foldr lcm 1 . walkAll . parse
  where walkAll dt = [walkFrom node (('Z'==).last) dt | (node,_)<-toList (snd dt), 'A'==last node]

walkFrom start isEnd (dirs,table) = walk (cycle dirs, start)
  where walk (d:ds, node)
          | isEnd node = 0
          | otherwise  = 1 + walk (ds, (if d=='L' then fst else snd) (table!node))

parse :: String -> ([Char], Map String (String,String))
parse = fmap (fromList . map perLine . lines) . split "\n\n"
  where perLine = fmap (split ", " . init . tail) . split " = "
        split str = (\(a:b:_) -> (a,b)) . splitOn str


{-NOTE old solution
walkFrom start isEnd (dirs,table) = length $ takeWhile (not.isEnd.snd) $ walk
  where walk = iterate step (cycle dirs, start)
        step (d:ds, node) = (ds, (if d=='L' then fst else snd) (table ! node))
-}

{-NOTE old solution
type Node = String

walkFrom :: Node -> (Node -> Bool) -> ([Char], Map Node (Node,Node)) -> Int
walk start isEnd (dirs, table) = go start (cycle dirs)
  where go node (d:dirs)
          | isEnd node = 0
          | otherwise  = 1 + go (step d node) dirs
        step dir = (if dir=='L' then fst else snd) . (table !)
-}

{-NOTE old solution
walk :: Node -> (Node -> Bool) -> ([Char], Map Node (Node,Node)) -> Int
walk node isEnd (dirs,table) = findEnd (iterate advance (node,cycle dirs))
  where findEnd = maybe undefined id . findIndex (fst.isEnd)
        advance (node, dir:dirs) = (step dir node, dirs)
        step dir = (if dir=='L' then fst else snd) . (table !)
-}

{-NOTE old solution
walk :: Node -> (Node -> Bool) -> ([Char], Map Node (Node,Node)) -> Int
walk start isEnd (dirs,table) = (\(_,_,n) -> n) loop
  where loop = until (\(_,node,_) -> isEnd node) advance (cycle dirs, start, 0)
        advance (dir:dirs, node, n) = (dirs, step dir node, n+1)
        step dir = (if dir=='L' then fst else snd) . (table !)
-}

{-NOTE bruteforce solution
solve2 = fst . head . snd . walk . parse
  where walk (dirs,table) = let
            done (nodes,_) = all (('Z'==).last) nodes
            step (nodes,(_,dir):insns) = ((if dir=='L' then fst else snd) <$> (table!) <$> nodes, insns)
          in until done step ([node | (node,_)<-toList table, 'A'==last node], zip [0..] (cycle dirs))

(!>) :: Eq a => [(a,b)] -> a -> b
xys !> x = maybe undefined id (lookup x xys)
-}
