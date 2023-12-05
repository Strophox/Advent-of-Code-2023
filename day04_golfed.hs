import Data.List (intersect)
import Control.Arrow (first,(&&&))

main = (solve1 &&& solve2) <$> readFile "04.txt" >>= print

solve1 = sum . map ((2^) . pred) . filter (>0) . parse

solve2 = fold . map ((,) 1) . parse
  where fold [] = 0
        fold ((cs,v):rest) = cs + fold (zipWith (first . (+)) (replicate v cs ++ repeat 0) rest)

parse = map (length . uncurry intersect . break (=="|") . words . drop 9) . lines
