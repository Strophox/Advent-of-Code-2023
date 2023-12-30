import Data.List
import Data.List.Split (splitOn)
import Data.Array
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Debug.Trace
logx str x = trace (str<>" :: "<>show x) x
logf str f x = trace (str<>" :: "<>show x<>" -> _") (f x)
logfx str f x = trace (str<>" :: "<>show x<>" -> "<>show (f x)) (f x)

main = let day = "17" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 txt = calcDistance grid [1,2,3]
  where grid = parse txt

solve2 txt = calcDistance grid [4..10]
  where grid = parse txt

data Dir = Vert | Hrzt
  deriving (Eq,Ord ,Show)

data Int2 = I2 Int Int
  deriving (Eq,Ord,Ix ,Show)

infixl 6 .+
(.+) :: Int2 -> Int2 -> Int2
(I2 a b) .+ (I2 c d) = I2 (a+c) (b+d)

infixl 7 .*
(.*) :: Int -> Int2 -> Int2
c .* (I2 a b) = I2 (c*a) (c*b)

calcDistance :: Array Int2 Int -> [Int] -> Int
calcDistance grid muls = dijkstra neighbors [(I2 0 0,Hrzt),(I2 0 0,Vert)] ((snd (bounds grid)==).fst)
  where neighbors (coord,dir) = let
            (dv,newdir) = case dir of Hrzt -> (I2 1 0, Vert)
                                      Vert -> (I2 0 1, Hrzt)
          in [(dist, (newcoord,newdir)) | m<-muls++map negate muls,
              let newcoord = m .* dv .+ coord,
              inRange (bounds grid) newcoord,
              let s = signum m,
              let dist = sum [grid ! (n .* dv .+ coord) | n<-[s,2*s..m]] ]

parse :: String -> Array Int2 Int
parse txt = listArray (I2 0 0, I2 n n) [read [c] | c<-txt, c/='\n']
  where n = length (lines txt) - 1

--dijkstra :: Ord a => (a -> [(Int,a)]) -> [a] -> (a -> Bool) -> Int--M.Map a Int
dijkstra neighbors starts isStop = run M.empty (S.fromList [(0,n) | n<-starts]) where
  --run dists pqueue = logf "run" (uncurry run') (dists,pqueue)
  run dists pqueue
    | S.null pqueue = error "no solution found"--dists
    | node`M.member`dists = run dists rest
    | isStop node = dist--M.insert node dist dists
    | otherwise   = run dists' pqueue'
    where Just ((dist,node),rest) = S.minView pqueue
          --(dist,node) = logx"c"(dist',node')
          newNodes = [(dist+d,n) | (d,n)<-neighbors node, n`M.notMember`dists]
          dists'  = M.insert node dist dists
          pqueue' = foldr S.insert rest newNodes
--(if fst node == I2 2 10 then \x -> trace ("nbrs :: "<>show (dists M.! (I2 4 10,Vert),node,n,dists M.! node,x)) x else id)$
