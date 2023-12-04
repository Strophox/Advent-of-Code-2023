import Data.List
import Data.Function
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map

main = let day = "03" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 txt = sum . map fst . filter isValidNum . fst . parse $ txt
  where isValidNum = any (`notElem`".0123456789") . map charAt . adjacent maxY maxX . snd
        charAt (y,x) = lines txt !! y !! x
        (maxY,maxX) = (length (lines txt) - 1, length (head (lines txt)) - 1)

solve2 txt = sum . map product . filter ((==3).length) . map nub . (\(nums,stars) -> let numMap = makeMap nums in [flip (Map.findWithDefault 1) numMap <$> adjacent maxY maxX (y,(x,x)) | (y,x)<-stars]) . parse $ txt
  where makeMap = Map.fromList . concatMap expand
        expand (num, (y,(x0,x1))) = [((y,x),num) | x<-[x0..x1]]
        (maxY,maxX) = (length (lines txt) - 1, length (head (lines txt)) - 1)

adjacent :: Int -> Int -> (Int,(Int,Int)) -> [(Int,Int)]
adjacent maxY maxX (y,(x0,x1)) = filter valid $
          [(y,x0-1),(y-1,x0-1),(y+1,x0-1)]
          ++ [(y,x1+1),(y-1,x1+1),(y+1,x1+1)]
          ++ map ((,) (y-1)) [x0..x1]
          ++ map ((,) (y+1)) [x0..x1]
  where valid (y,x) = 0<=y && y<=maxY && 0<=x && x<=maxX

-- (numcoords,starcoords) = ([(n,(y,(x0,x1)))], [(y,x)])
parse :: String -> ([(Int, (Int,(Int,Int)))], [(Int,Int)])
parse = (concatMap fst &&& concatMap snd) . map (uncurry perLine) . zip [0..] . lines
  where perLine y = (map (perDigitgroup y) . filter (isDigit . head) &&& map (perStargroup y) . filter (isStar . head)) . groupBy eqRel . zip [0..]
        eqRel x y = ((&&)`on`isDigit) x y || ((&&)`on`isStar) x y || ((&&)`on`isNeither) x y
        isDigit = (`elem`"0123456789") . snd
        isStar = (=='*') . snd
        isNeither = (`notElem`"0123456789*") . snd
        perDigitgroup y = read . map snd &&& (const y &&& (fst . head &&& fst . last))
        perStargroup y = const y &&& fst . head
