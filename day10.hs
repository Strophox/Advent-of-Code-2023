import Data.Array
import Data.Map.Strict (Map,insert,singleton,notMember)
import qualified Data.Map.Strict as Map

main = let day = "10" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = maximum . Map.elems . uncurry runBfs . parse

solve2 txt = length . filter (odd . crossings 0) . filter (`notMember`dists) $ indices grid
  where (grid,start) = parse txt
        dists = runBfs grid start
        crossings n (x,y)
          | x < 0     = n
          | (x,y)`notMember`dists || grid!(x,y) `notElem` "|LJ"
                      = crossings n (x-1,y)
          | otherwise = crossings (n+1) (x-1,y)

runBfs :: Array (Int,Int) Char -> (Int,Int) -> Map (Int,Int) Int
runBfs grid start = bfs (singleton start 0) [start]
  where bfs dists []           = dists
        bfs dists (node:queue) = bfs dists' (queue++newNodes)
          where newNodes = filter (`notMember`dists) (neighbors node)
                dists'   = foldr (`insert`(dists Map.! node + 1)) dists newNodes
        neighbors (x,y) = case grid!(x,y) of
          'L' -> [u,r] ; '-' -> [l,r] ; 'J' -> [l,u]
          'F' -> [d,r] ; '|' -> [d,u] ; '7' -> [d,l]
          where (d,l,u,r) = ((x,y+1),(x-1,y),(x,y-1),(x+1,y))

parse :: String -> (Array (Int,Int) Char, (Int,Int))
parse = fixStart . makeGrid . makeAssocs
  where makeAssocs txt = [((x,y),c) | (y,line)<-zip [0..] (lines txt), (x,c)<-zip [0..] line]
        makeGrid assocs = (array ((0,0),fst (last assocs)) assocs, head [xy | (xy,c)<-assocs, c=='S'])
        fixStart (grid,start@(x,y)) = let
            dirVal = 1 * fromEnum (grid!(x+1,y)`elem`"7-J")
                   + 2 * fromEnum (grid!(x,y-1)`elem`"7|F")
                   + 4 * fromEnum (grid!(x-1,y)`elem`"L-F")
                   + 8 * fromEnum (grid!(x,y+1)`elem`"J|L")
            fixedGrid = grid // [(start,"...L.-J..F|.7..." !! dirVal)]
          in (fixedGrid,start)

{-NOTE old solution
solve2 txt = length . filter isInside . filter (`notMember`dists) $ A.indices arr
  where (arr,start) = parse txt
        dists = runBfs arr start
        isInside node = odd (crossings node (Neutral,0))
        crossings :: (Int,Int) -> (State,Int) -> Int
        crossings (x,y) (state, n)
          | x < 0                 = n
          | (x,y)`notMember`dists = crossings (x-1,y) (state, n)
          | otherwise             = crossings (x-1,y) (case arr A.! (x,y) of
            '7' -> (FromBot, n)
            'L' -> (Neutral, if state==FromBot then n+1 else n)
            'J' -> (FromTop, n)
            'F' -> (Neutral, if state==FromTop then n+1 else n)
            '|' -> (Neutral, n+1)
            _   -> (state, n) )

data State = FromTop | FromBot | Neutral
  deriving (Eq)
-}
