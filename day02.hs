import Data.List.Split (splitOn)
import Control.Arrow ((&&&),(***))

main = let day = "02" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 :: String -> Int
solve1 = sum . map fst . filter (all valid . snd) . parse
  where valid (r,g,b) = r <= 12 && g <= 13 && b <= 14

solve2 :: String -> Int
solve2 = sum . map perGame . parse
  where perGame = product' . foldr1 max' . snd
        max' (r,g,b) (r',g',b') = (max r r', max g g', max b b')
        product' (r,g,b) = r * g * b

parse :: String -> [(Int,[(Int,Int,Int)])]
parse = map perLine . lines
  where perLine = (read *** (map perSet . splitOn "; " . drop 1)) . break (==':') . drop 5
        perSet = foldr build (0,0,0) . map perEntry . splitOn ", "
        perEntry = ((read . (!!0)) &&& (!!1)) . words
        build (i,color) (r,g,b) = case color of
          "red"   -> (r+i,g,b)
          "green" -> (r,g+i,b)
          "blue"  -> (r,g,b+i)


{-NOTE old solution

solve1 :: String -> Int
solve1 = sum . map fst . filter (all valid . snd) . parse
  where valid cset = red cset <= 12 && green cset <= 13 && blue cset <= 14

solve2 :: String -> Int
solve2 = sum . map perGame . parse
  where perGame = multiply . foldr max' (0,0,0) . snd
        max' c (r,g,b) = (max (red c) r, max (green c) g, max (blue c) b)
        multiply (r,g,b) = r * g * b

data CSet = CSet
  { red   :: Int
  , green :: Int
  , blue  :: Int
  }

parse :: String -> [(Int,[CSet])]
parse = map perLine . lines
  where perLine = (read *** (map perSet . splitOn "; " . drop 1)) . break (==':') . drop 5
        perSet = foldr build (CSet 0 0 0) . map perEntry . splitOn ", "
        perEntry = ((read . (!!0)) &&& (!!1)) . words
        build (i,color) cset = case color of
          "red" -> cset { red = i }
          "blue" -> cset { blue = i }
          "green" -> cset { green = i }
-}
