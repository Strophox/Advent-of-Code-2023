import Data.List
import Data.List.Split (splitOn)
import Control.Monad ((>=>))
import Control.Arrow ((>>>),(&&&),(***))
--import Debug.Trace
--dbg marker x = trace ("> "<>marker<>" "<>show x) x

main = let day = "05" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = minimum . applyAll . (id *** chainl . map converter) . parse
  where applyAll (seeds, f) = map f seeds
        converter :: [(Int,Int,Int)] -> Int -> Int
        converter [] v = v
        converter ((dst,src,len):mappings) v
          | src <= v&&v < src+len = v - src + dst
          | otherwise             = converter mappings v

solve2 = fst . minimum . mapplyAll . (intervals *** bindl . map iconverter) . parse
  where mapplyAll (vals, m) = concatMap m vals
        iconverter :: [(Int,Int,Int)] -> (Int,Int) -> [(Int,Int)]
        iconverter [] i = [i]
        iconverter ((dst,src,len):mappings) i =
          let (moved,leftovers) = translate (dst-src) (src,src+len-1) i
          in moved ++ concatMap (iconverter mappings) leftovers
          where
            translate d (l,r) (il,ir)
              | r< il || ir <l = ([], [(il,ir)])
              | l<=il && ir<=r = ([(il+d,ir+d)], [])
              | l<=il && r <ir = ([(il+d,r+d)], [(r+1,ir)])
              | il< l && ir<=r = ([(l+d,ir+d)], [(il,l-1)])
              | il< l && r <ir = ([(l+d,r+d)], [(il,l-1),(r+1,ir)])
        intervals [] = []
        intervals (start:len:rest) = (start,start+len-1) : intervals rest

parse :: String -> ([Int], [[(Int,Int,Int)]])
parse = (parseSeeds . head &&& map parseMap . tail) . splitOn "\n\n"
  where parseSeeds = map read . words . drop 6
        parseMap = map perLine . tail . lines
        perLine = (\[dst,src,len] -> (dst,src,len)) . map read . words

chainl :: Foldable t => t (a -> a) -> (a -> a)
chainl = foldl (>>>) id

bindl :: (Foldable t,Monad m) => t (a -> m a) -> a -> m a
bindl = foldl (>=>) return

{-NOTE unused helpers
chainr :: Foldable t => t (a -> a) -> (a -> a)
chainr = foldr (.) id

bindr :: (Foldable t,Monad m) => t (a -> m a) -> a -> m a
bindr = foldr (<=<) return
-}

{-NOTE old solution
solve1 = minimum . doMapping . parse
  where doMapping (seeds,maps) = map (chainl (converter <$> maps)) seeds
        converter :: [(Int,Int,Int)] -> Int -> Int
        converter mapping v = foldr (\(dst,src,len) x -> if src<=v&&v<src+len then v-src+dst else x) v mapping
-}
