import Data.List (intersect)
import Control.Arrow (first,(***))

main = let day = "04" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = sum . map (pow0 . length . uncurry intersect) . parse
  where pow0 0 = 0
        pow0 i = 2^(i-1)

solve2 = cascade . map ((,) 1 . length . uncurry intersect) . parse
  where cascade [] = 0
        cascade ((cards,score):rest) = cards + cascade (zipWith (first . (+)) (replicate score cards ++ repeat 0) rest)

parse :: String -> [([Int], [Int])]
parse = map perLine . lines
  where perLine = (map read *** map read . drop 1) . break (=="|") . words . drop 9


{-NOTE old solution

solve1 = sum . map perCard . parse
  where perCard = (\i -> if i==0 then 0 else 2^(i-1)) . length . uncurry intersect

solve2 = fst . fst . cascade . map (length . uncurry intersect) . parse
  where cascade vals = until (null . snd) step ((0, vals), const 1 <$> vals)
        step ((tally, s:ss), c:cs) = ((tally+c, ss), zipWith (+) (replicate s c ++ repeat 0) cs)-}
