import Control.Arrow ((&&&))

main = let day = "06" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = product . map perRace . parse1
  where perRace (t,d) = length [t' | t'<-[0..t], t'*(t-t') > d]

solve2 = (\[x0,x1] -> ceiling x1 - floor x0 - 1) . roots . parse2
  where roots (t,d) =
          [ (-t + sqrt (t^2 - 4*d)) / (-2)
          , (-t - sqrt (t^2 - 4*d)) / (-2)
          ]

parse1 :: String -> [(Int,Int)]
parse1 = uncurry zip . (head &&& last) . map (map read . tail . words) . lines

parse2 :: String -> (Double,Double)
parse2 = (head &&& last) . map (read . filter (/=' ') . drop 9) . lines

{-NOTE old solution, naïve part 2
solve1 = solveWith parse1

solve2 = solveWith parse2

solveWith parse = product . map perRace . parse
  where perRace (t,d) = length [t' | t'<-[0..t], t'*(t-t') > d]

parse1 :: String -> [(Int,Int)]
parse1 = uncurry zip . (head &&& last) . map (map read . tail . words) . lines

parse2 :: String -> [(Int,Int)]
parse2 = (:[]) . (head &&& last) . map (read . filter (/=' ') . drop 9) . lines
-}

{-NOTE on the problem statement
Given t,d we solve for all times x with
  x * (t - x) > d
Accomplish this by using
  ax² + bx + c = 0
==>
  x = (-b +- sqrt (b^2 - 4*a*c)) / (2*a)
and applying it on
  tx - x² - d = 0  |  a = -1, b = t, c = -d
<=>
  x = (-t +- sqrt (t^2 - 4*d)) / (-2)
-}
