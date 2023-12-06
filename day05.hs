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

solve1 = minimum . mapAll . (id *** chainr . map converter) . parse
  where mapAll (seeds, f) = map f seeds
        converter :: [(Int,Int,Int)] -> Int -> Int
        converter [] v = v
        converter ((dst,src,len):mappings) v
          | src <= v&&v < src+len = (+) (dst-src) v
          | otherwise             = converter mappings v

solve2 = fst . minimum . catMapAll . (intervals *** bindr . map iconverter) . parse
  where catMapAll (vals, m) = concatMap m vals
        iconverter :: [(Int,Int,Int)] -> (Int,Int) -> [(Int,Int)]
        iconverter [] i = [i]
        iconverter ((dst,src,len):mappings) i =
          let (moved,leftovers) = mapWithin (src,src+len-1) (dst-src) i
          in moved ++ concatMap (iconverter mappings) leftovers
          where
            mapWithin (l,r) d (il,ir)
              | r< il || ir <l = ([], [(il,ir)])
              | l<=il && ir<=r = ([(il+d,ir+d)], [                 ])
              | l<=il && r <ir = ([(il+d, r+d)], [         (r+1,ir)])
              | il< l && ir<=r = ([( l+d,ir+d)], [(il,l-1)         ])
              | il< l && r <ir = ([( l+d, r+d)], [(il,l-1),(r+1,ir)])
        intervals [] = []
        intervals (start:len:rest) = (start,start+len-1) : intervals rest

parse :: String -> ([Int], [[(Int,Int,Int)]])
parse = (parseSeeds . head &&& map parseMap . tail) . splitOn "\n\n"
  where parseSeeds = map read . words . drop 6
        parseMap = map perLine . tail . lines
        perLine = (\[dst,src,len] -> (dst,src,len)) . map read . words

chainr :: Foldable t => t (a -> a) -> (a -> a)
chainr = foldr (>>>) id

bindr :: (Foldable t,Monad m) => t (a -> m a) -> a -> m a
bindr = foldr (>=>) return

{-NOTE old solution
solve1 = minimum . applyMaps . parse
  where applyMaps (seeds,maps) = map (chainr (converter <$> maps)) seeds
        converter mapping v = foldr (\(dst,src,len) x -> if src<=v&&v<src+len then v-src+dst else x) v mapping
-}

{-NOTE unused helpers
chainl :: Foldable t => t (a -> a) -> (a -> a)
chainl = foldr (.) id

bindl :: (Foldable t,Monad m) => t (a -> m a) -> a -> m a
bindl = foldr (<=<) return

-- NOTICE that
--   foldl (.) id  .=.  foldr (.) id
-- i.e.
--   chainl .=. chainr
-- Therefore we instead use 'l' and 'r' to denote *direction* of composition
--   chainl [f,g,h] = (f . g . h)
--   chainr [f,g,h] = (f >>> g >>> h)
--   bindl [f,g,h] = (f <=< g <=< h)
--   bindr [f,g,h] = (f >=> g >=> h)
-- Maybe we should call them
--   chainf, bindf (function; or forward..)
--   chainb, bindb (binding; or backward..)
-- instead?? that may make it even more confusing xD
-}
