import Data.List

day = "01"

main = do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- init <$> readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

aux :: (String -> [Char]) -> String -> Int
aux lineToDigits = sum . map (read . (\ds -> [head ds,last ds]) . lineToDigits) . lines

solve1 :: String -> Int
solve1 = aux (filter (`elem`['0'..'9']))

solve2 :: String -> Int
solve2 = aux (concatMap suffixMatches . inits)
  where suffixMatches str = [d | (token,d)<-dict, token`isSuffixOf`str]
        dict = zip  ["0","1","2","3","4","5","6","7","8","9","zero","one","two","three","four","five","six","seven","eight","nine"] (cycle ['0'..'9'])

{-NOTE old, parses "oneight" as only "1"
lineDigits2 :: String -> String
lineDigits2 = fst . foldl step ([],[])
  where step (ds,cs) c = maybe (ds,cs++[c]) (\d -> (ds++[d],[])) (dict`parseMatch`(cs++[c]))
        dict = zip ["0","1","2","3","4","5","6","7","8","9","zero","one","two","three","four","five","six","seven","eight","nine"] (cycle ['0'..'9'])

parseMatch :: [(String,Char)] -> String -> Maybe Char
parseMatch dict str = listToMaybe [d | (token,d)<-dict, token`isSuffixOf`str]
-}
