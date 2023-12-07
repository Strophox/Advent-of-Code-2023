import Data.List (intersect)
import Control.Arrow (first,(&&&))

main = (solve1 &&& solve2) <$> parse <$> readFile "04.txt" >>= print

parse = map (length . uncurry intersect . break (=="|") . words . drop 9) . lines

solve1 = sum . map ((2^) . pred) . filter (>0)

solve2 = cascade . map ((,) 1)
  where cascade [] = 0
        cascade ((cs,v):rest) = cs + cascade (zipWith (first . (+)) (replicate v cs ++ repeat 0) rest)
