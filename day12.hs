import Data.List
import Data.List.Split (splitOn)
import Data.Array
import Control.Arrow ((***),(&&&))

main = let day = "12" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = sum . map counts . parse
  where counts (str,nums) = length . filter ((nums==).groups) $ traverse branch str
        branch c = if c=='?' then ['.','#'] else [c]
        groups str = [1+length gs | '#':gs<-group str]

solve2 = sum . map (counts . fivetimes) . parse
  where fivetimes = (intercalate "?" . replicate 5 *** concat . replicate 5)

counts :: ([Char], [Int]) -> Int
counts (str',nums') = table ! (c1,n1,False)
  where
    (str,nums) = ("."<>str'<>".", 0:nums')
    (c1,n1) = (length str - 1, length nums - 1)
    table = let bounds = ((0,0,False),(c1,n1,True))
            in listArray bounds (rec <$> range bounds)
    rec (c,n,b)
      | c == 0                    = if n == 0 && not b then 1 else 0
      | not b && str!!c`elem`".?" = table!(c-1,n,True) + table!(c-1,n,False)
      |     b && str!!c`elem`"#?" = if canTake then table!(c-len,n-1,False) else 0
      | otherwise                 = 0
      where len = nums !! n
            canTake = let slice = take len (drop (c-len+1) str)
                      in n /= 0 && length slice == len && all (/='.') slice

parse :: String -> [([Char], [Int])]
parse = map perLine . lines
  where perLine = (head &&& map read . splitOn "," . last) . words

{-NOTE old debug stuff
import Debug.Trace
dbg s v = trace ("> "<>s<>show v) v
call fname f x = let y = f x in trace ("> "<>fname<>" "<>show x<>" = "<>show y) y
main = readFile "12.txt" >>= {-putStrLn . fmt . head-}print . head . map counts . parse
fmt arrDP = intercalate "\n\n" [fmt' True, fmt' False]
  where fmt' b = intercalate "\n" (map (line b) [0..3])
        line b n = intercalate " " (map (\c -> show $ arrDP!(c,n,b)) [0..8])
-}

{-NOTE unused helper -- also Data.Array.IArray not up-to-date
--genArray :: (IArray a e, Ix i)=> (i,i) -> (i -> e) -> a i e
genArray bounds f = listArray bounds (f <$> range bounds)
-}

{-NOTE old pt1 solution
solve1 = sum . map perRow . parse
  where perRow (chars,nums) = length . filter (valid nums) . sequence $ branch <$> chars
        branch c = if c=='?' then ['.','#'] else [c]
        valid nums str = nums == [1+length gs | '#':gs<-group str]
-}
