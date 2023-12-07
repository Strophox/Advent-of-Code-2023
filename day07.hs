import Data.List
import Control.Arrow ((***),(&&&))

main = let day = "07" in do
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  txt <- readFile (day<>".txt")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = sum . zipWith (*) [1..] . map snd . sortOn ((handValue &&& map (`getIndex`"23456789TJQKA")) . fst) . parse
  where handValue = reverse . sort . map length . group . sort

solve2 = sum . zipWith (*) [1..] . map snd . sortOn ((handValue &&& map (`getIndex`"J23456789TQKA")) . fst) . parse
  where handValue = (\(j,cs) -> case cs of [] -> [j]; c:cs -> c+j:cs)
                    . (length *** reverse . sort . map length . group . sort)
                    . partition (=='J')

parse :: String -> [([Char], Int)]
parse = map ((head &&& read . last) . words) . lines

getIndex :: Eq a => a -> [a] -> Int
getIndex x xs = maybe (error "not found") id (elemIndex x xs)

{-NOTE old solution
solve1 = sum . map (\(i,(_,b)) -> i * b) . zip [1..] . sortOn (evalHand . fst) . parse
  where
    evalHand = ( (`getIndex`types) . handType
               &&& map (`getIndex`"23456789TJQKA") )
    handType = sort . map length . group . sort

solve2 = sum . map (\(i,(_,b)) -> i * b) . zip [1..] . sortOn (evalHand . fst) . parse
  where
    evalHand = ( (`getIndex`types) . handType
               &&& map (`getIndex`"J23456789TQKA") )
    handType = (\(j,cs) -> if null cs then [j] else init cs ++ [j+last cs])
               . (length *** sort . map length . group . sort)
               . partition (=='J')

getIndex :: Eq a => a -> [a] -> Int
getIndex x xs = maybe (error "not found") id (elemIndex x xs)

types = [ [1,1,1,1,1] -- High card
        , [1,1,1,2]   -- One pair
        , [1,2,2]     -- Two pair
        , [1,1,3]     -- Three of a kind
        , [2,3]       -- Full house
        , [1,4]       -- Four of a kind
        , [5]         -- Five of a kind
        ]
-}

{-NOTE old solution
--import Data.Function (on)

--...

solve1 = sum . map (\(i,(_,b)) -> i * b) . zip [1..] . sortOn fst . parse

solve2 = sum . map (\(i,(_,b)) -> i * b) . zip [1..] . sortOn fst . parse2

newtype Card = Card { char :: Char }
  deriving (Eq)

newtype Hand = Hand { cards :: [Card] }
  deriving (Eq)

instance Ord Card where
  compare = compare`on`eval
    where eval = maybe undefined id . (`elemIndex`"23456789TJQKA") . char

instance Ord Hand where
  compare a b = (compare`on`eval) a b <> (compare`on`cards) a b
    where eval = maybe undefined id . (`elemIndex`types) . sort . map length . group . sort . cards

parse :: String -> [(Hand, Int)]
parse = map perLine . lines
  where perLine = (Hand . map Card . head &&& read . last) . words


newtype Card2 = Card2 { char2 :: Char }
  deriving (Eq)

newtype Hand2 = Hand2 { cards2 :: [Card2] }
  deriving (Eq)

instance Ord Card2 where
  compare = compare`on`eval
    where eval = maybe undefined id . (`elemIndex`"J23456789TQKA") . char2

instance Ord Hand2 where
  compare a b = (compare`on`eval) a b <> (compare`on`cards2) a b
    where eval = maybe (error "hi") id . (`elemIndex`types) . (\(j,cs) -> if null cs then [j] else init cs ++ [j+last cs]) . (length *** sort . map length . group . sort) . partition (== Card2 'J') . cards2

types = [ [1,1,1,1,1] -- High card
        , [1,1,1,2]   -- One pair
        , [1,2,2]     -- Two pair
        , [1,1,3]     -- Three of a kind
        , [2,3]       -- Full house
        , [1,4]       -- Four of a kind
        , [5]         -- Five of a kind
        ]

parse2 :: String -> [(Hand2, Int)]
parse2 = map perLine . lines
  where perLine = (Hand2 . map Card2 . head &&& read . last) . words
-}
