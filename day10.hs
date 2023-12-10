import qualified Data.Array as A
import Data.Map.Strict (Map,(!),insert,singleton,notMember,elems)

main = let day = "10" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = maximum . elems . uncurry runBfs . parse

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

runBfs :: A.Array (Int,Int) Char -> (Int,Int) -> Map (Int,Int) Int
runBfs arr start = bfs (singleton start 0) [start]
  where bfs dists []           = dists
        bfs dists (node:queue) = let
            newNodes = filter (`notMember`dists) (neighbors node)
            dists' = foldr (`insert`(dists!node + 1)) dists newNodes
          in bfs dists' (queue++newNodes)
        neighbors (x,y) =
          let (l,r,u,d) = ((x-1,y),(x+1,y),(x,y-1),(x,y+1))
          in case arr A.! (x,y) of
            '7' -> [l,d]
            'L' -> [r,u]
            'J' -> [l,u]
            'F' -> [r,d]
            '|' -> [u,d]
            '-' -> [l,r]

parse :: String -> (A.Array (Int,Int) Char, (Int,Int))
parse = fixStart . makeArray . makeAssocs
  where makeAssocs txt = [((x,y),char) | (y,line)<-zip [0..] (lines txt), (x,char)<-zip [0..] line]
        makeArray assocs = (A.array ((0,0),fst (last assocs)) assocs, head [xy | (xy,char)<-assocs, char=='S'])
        fixStart (arr,start@(x,y)) = let
            dirVal = (if arr A.! (x+1,y) `elem` "7-J" then 1 else 0)
                   + (if arr A.! (x,y-1) `elem` "7|F" then 2 else 0)
                   + (if arr A.! (x-1,y) `elem` "L-F" then 4 else 0)
                   + (if arr A.! (x,y+1) `elem` "J|L" then 8 else 0)
            fixedArr = arr A.// [(start,"...L.-J..F|.7..." !! dirVal)]
          in (fixedArr,start)
