import Data.List

main = let day = "01" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 :: String -> Int
solve1 = solveWith (map pure ['1'..'9'])

solve2 :: String -> Int
solve2 = solveWith (map pure ['1'..'9'] ++ ["one","two","three","four","five","six","seven","eight","nine"])

solveWith :: [String] -> String -> Int
solveWith tokens = sum . map perLine . lines
  where perLine = (\ds -> read [head ds,last ds]) . concatMap prefixMatches . tails
        prefixMatches str = [digit | (token,digit) <- tokens`zip`cycle ['1'..'9'], token`isPrefixOf`str]

{-NOTE solution variant
aux :: (String -> [Char]) -> String -> Int
aux lineToDigits = sum . map (read . (\ds -> [head ds,last ds]) . lineToDigits) . lines

solve1 :: String -> Int
solve1 = aux (filter (`elem`['0'..'9']))

solve2 :: String -> Int
solve2 = aux (concatMap prefixMatches . inits)
  where prefixMatches str = [digit | (token,digit)<-dict, token`isPrefixOf`str]
        dict = zip  ["0","1","2","3","4","5","6","7","8","9","zero","one","two","three","four","five","six","seven","eight","nine"] (cycle ['0'..'9'])
-}

{-NOTE previous, faulty - parses "oneight" as only "1" because discarding
lineDigits2 :: String -> String
lineDigits2 = fst . foldl step ([],[])
  where step (ds,cs) c = maybe (ds,cs++[c]) (\d -> (ds++[d],[])) (dict`parseMatch`(cs++[c]))
        dict = zip ["0","1","2","3","4","5","6","7","8","9","zero","one","two","three","four","five","six","seven","eight","nine"] (cycle ['0'..'9'])

parseMatch :: [(String,Char)] -> String -> Maybe Char
parseMatch dict str = listToMaybe [d | (token,d)<-dict, token`isSuffixOf`str]
-}
