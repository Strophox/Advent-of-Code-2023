import Data.List
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))

main = let day = "13" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = sum . map (\(h,v) -> 100*h + v) . map (find &&& find . transpose) . parse
  where find = findMirrorBy (\(xs,ys) -> and (zipWith (==) xs ys))

solve2 = sum . map (\(h,v) -> 100*h + v) . map (find &&& find . transpose) . parse
  where find = findMirrorBy (\(xs,ys) -> 1 == sum (zipWith lineDiff xs ys))
        lineDiff x y = sum $ fromEnum <$> zipWith (/=) x y

findMirrorBy :: Eq a => (([a],[a]) -> Bool) -> [a] -> Int
findMirrorBy cmp = sum . map succ . findIndices cmp . init . tail . splits
  where splits = zip <$> tails <*> reverse . tails . reverse

parse :: String -> [[[Char]]]
parse = map lines . splitOn "\n\n"


{-NOTE initial solve1

solve1 = uncurry (+)
      . ((100*) . sum *** sum)
      . (concatMap mirrorings &&& concatMap (mirrorings . transpose)) . parse
  where mirrorings = init . tail . findIndices equalSided . mirrorSplits
        equalSided (xs,ys) = and (zipWith (==) xs ys)
        mirrorSplits = zip <$> tails <*> reverse . tails . reverse-}
