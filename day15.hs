import Data.List
import Data.List.Split (splitOn)

main = let day = "15" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = sum . map runHash . parse

solve2 = sum . concat
       . zipWith (\n box -> zipWith (*) [1..] $ (n*) . read . snd <$> box) [1..]
       . foldl builds (replicate 256 []) . map (break (`elem`"-=")) . parse
  where builds :: [[(String,String)]] -> (String,String) -> [[(String,String)]]
        builds boxes (lbl,ins) = let (as,(b:cs)) = splitAt (runHash lbl) boxes
          in as ++ build b (lbl,ins) : cs
        build :: [(String,String)] -> (String,String) -> [(String,String)]
        build box (lens,ins) = case ins of
          "-"     -> remove lens box
          '=':val -> update lens val box
        remove lens [] = []
        remove lens ((k,v):kvs)
          | k == lens = kvs
          | otherwise = (k,v) : remove lens kvs
        update lens val [] = [(lens,val)]
        update lens val ((k,v):kvs)
          | k == lens = (k,val) : kvs
          | otherwise = (k,v) : update lens val kvs

runHash :: [Char] -> Int
runHash = foldl hash 0
  where hash a c = (a + fromEnum c) * 17 `mod` 256

parse :: String -> [String]
parse = splitOn "," . head . lines


{-NOTE comparison of solutions

-- All boxes at once, arrays
-- (0.10 secs, 40,960,768 bytes)
import Data.Array
solve2 = sum . concat
       . zipWith (\n box -> zipWith (*) [1..] $ (n*) . read . snd <$> box) [1..]
       . elems
       . foldl builds (listArray (0,255) (replicate 256 [])) . map (break (`elem`"-=")) . parse
  where builds :: Array Int [(String,String)] -> (String,String) -> Array Int [(String,String)]
        builds boxes (lbl,ins) = let n = runHash lbl
          in boxes // [(n, build (boxes!n) (lbl,ins))]
        build :: [(String,String)] -> (String,String) -> [(String,String)]
        build box (lens,ins) = case ins of
          "-"     -> remove lens box
          '=':val -> update lens val box
        remove lens [] = []
        remove lens ((k,v):kvs)
          | k == lens = kvs
          | otherwise = (k,v) : remove lens kvs
        update lens val [] = [(lens,val)]
        update lens val ((k,v):kvs)
          | k == lens = (k,val) : kvs
          | otherwise = (k,v) : update lens val kvs

-- All boxes at once, lists
-- (0.17 secs, 129,238,440 bytes)
solve2 = sum . concat
       . zipWith (\n box -> zipWith (*) [1..] $ (n*) . read . snd <$> box) [1..]
       . foldl builds (replicate 256 []) . map (break (`elem`"-=")) . parse
  where builds :: [[(String,String)]] -> (String,String) -> [[(String,String)]]
        builds boxes (lbl,ins) = let (as,(b:cs)) = splitAt (runHash lbl) boxes
          in as ++ build b (lbl,ins) : cs
        build :: [(String,String)] -> (String,String) -> [(String,String)]
        build box (lens,ins) = case ins of
          "-"     -> remove lens box
          '=':val -> update lens val box
        remove lens [] = []
        remove lens ((k,v):kvs)
          | k == lens = kvs
          | otherwise = (k,v) : remove lens kvs
        update lens val [] = [(lens,val)]
        update lens val ((k,v):kvs)
          | k == lens = (k,val) : kvs
          | otherwise = (k,v) : update lens val kvs

-- All boxes separately
-- (2.32 secs, 1,070,509,336 bytes)
solve2 = sum . concat
       . zipWith (\n box -> zipWith (*) [1..] $ (n*) . read . snd <$> box) [1..]
       . (\is -> makeBox is <$> [0..255]) . map (break (`elem`"-=")) . parse
  where makeBox is n = foldl build [] (filter ((n==).runHash.fst) is)
        build box (lens,ins) = case ins of
          "-"     -> remove lens box
          '=':val -> update lens val box
        remove lens [] = []
        remove lens ((k,v):kvs)
          | k == lens = kvs
          | otherwise = (k,v) : remove lens kvs
        update lens val [] = [(lens,val)]
        update lens val ((k,v):kvs)
          | k == lens = (k,val) : kvs
          | otherwise = (k,v) : update lens val kvs
-}

{-NOTE old solve2 parts
solve2 = sum . concat
       . map (zipWith (*) [1..]) . zipWith (map . (*)) [1..]
       . map (map (read.snd))
       ...

        remove lens = foldr (\(k,v) kvs -> if k==lens then kvs else (k,v):kvs) []
        remove lens = filter ((/=lens).snd)
-}
