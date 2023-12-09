main = let day = "09" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = sum . map extrapolate . parse

solve2 = sum . map (extrapolate . reverse) . parse

extrapolate :: [Int] -> Int
extrapolate ns
  | all (==0) ns = 0
  | otherwise    = last ns + extrapolate (diffs ns)
  where diffs ns = zipWith (-) (tail ns) ns

parse :: String -> [[Int]]
parse = map (map read . words) . lines

{-NOTE old solution
diffs (a:b:cs) = (b-a) : diffs (b:cs)
diffs _ = []
-}
