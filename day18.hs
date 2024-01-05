import Data.List
import Control.Arrow ((&&&))

main = let day = "18" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = solve' . map fst . parse

solve2 = solve' . map snd . parse

solve' = uncurry (+) . (negate . shoelace . toCoords &&& borderSum)
  where toCoords = scanl' (\piv (dir,n) -> piv .+ n .* dir) (0,0)
        borderSum = (`div`2) . (+2) . sum . map snd

shoelace :: [(Int,Int)] -> Int
shoelace ps = (`div`2) $ sum $ zipWith (\(x0,y0) (x1,y1) -> x0*y1 - x1*y0) ps (drop 1 $ cycle ps)

type Step = ((Int,Int),Int)

parse :: String -> [(Step, Step)]
parse = map perLine . lines
  where perLine = (\[[d],n,str] -> ((parseDir d,read n), parseRgb str)) . words
        parseDir d = case d of 'R'->(1,0); 'U'->(0,1); 'L'->(-1,0); 'D'->(0,-1)
        parseRgb = (parseDir' . last &&& read . ("0x"++) . take 5) . take 6 . drop 2
        parseDir' d = case d of '0'->(1,0); '3'->(0,1); '2'->(-1,0); '1'->(0,-1)

infixl 6 .+
(.+) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(a,b) .+ (c,d) = (a+c, b+d)

infixl 7 .*
(.*) :: Int -> (Int,Int) -> (Int,Int)
c .* (a,b) = (c*a, c*b)
