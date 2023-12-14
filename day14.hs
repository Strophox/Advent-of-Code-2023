import Data.List
import Data.Array
import qualified Data.Map.Strict as M

main = let day = "14" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = totalLoad . tilt . parse

solve2 = totalLoad . iter tiltCycle 1000000000 . parse
  where tiltCycle = rotate.tilt . rotate.tilt . rotate.tilt . rotate.tilt
        rotate arr = let bnd@(_,(_,x1)) = bounds arr
                     in ixmap bnd (\(y,x) -> (x1-x,y)) arr
        iter f n = go 0 M.empty where
          go i hist arr
            | i == n            = arr
            | arr`M.member`hist = go iSkip M.empty arr
            | otherwise         = go (i+1) (M.insert arr i hist) (f arr)
            where iSkip = n - (n - i)`mod`(i - hist M.! arr)

totalLoad :: Array (Int,Int) Char -> Int
totalLoad arr = sum [y1 - y | ((y,_),'O')<-assocs arr]
  where y1 = (fst . snd . bounds) arr + 1

tilt :: Array (Int,Int) Char -> Array (Int,Int) Char
tilt arr = foldl' roll arr (indices arr)
  where roll arr (y,x)
          | arr!(y,x)=='O' = arr // [((y,x),'.'), ((y',x),'O')]
          | otherwise      = arr
          where y' = last $ y : takeWhile (\i -> arr!(i,x)=='.') [y-1,y-2..0]

parse :: String -> Array (Int,Int) Char
parse txt = listArray ((0,0),(n,n)) (filter (/='\n') txt)
  where n = length (lines txt) - 1


{-NOTE alternative LOC
  where tiltCycle = rotate.tilt . rotate.tilt . rotate.tilt . rotate.tilt
  where tiltCycle = foldr1 (.) (replicate 4 (rotate . tilt))}
-}

{-NOTE old debug stuff
import Debug.Trace; dbg s v = trace ("> "<>s<>" "<>show v) v

main = do
  {-
  let arr = listArray ((0,0),(1,1)) "1234"
  putStrLn $ showArr $ arr
  putStrLn $ showArr $ rotate $ arr
  putStrLn $ showArr $ rotate $ rotate $ arr
  putStrLn $ showArr $ rotate $ rotate $ rotate $ arr
  putStrLn $ showArr $ rotate $ rotate $ rotate $ rotate $ arr
  -}
  txt <- readFile "14.txt"
  --putStrLn $ (showArr . parse) txt
  --putStrLn $ (showArr . tilt . parse) txt
  --traverse (putStrLn . showArr) (solve2 txt)
  where
    showArr arr = unlines $ map (map snd) $ groupBy ((==)`on`fst.fst) $ sort $ assocs arr
    rotate arr = let bnd@(_,(x',_)) = bounds arr
                 in ixmap bnd (\(y,x) -> (x'-x,y)) arr
    fourTilt = rotate . tilt . rotate . tilt . rotate . tilt . rotate . tilt
-}
