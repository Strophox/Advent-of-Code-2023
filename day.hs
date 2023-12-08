import Data.List
import Data.List.Split (splitOn)
import Control.Arrow ((***),(&&&))
import Debug.Trace; dbg s v = trace ("> "<>s<>" "<>show v) v

main = let day = undefined in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = parse

solve2 = const "todo"

parse :: String ->
parse =
