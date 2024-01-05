import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Control.Arrow ((***),(&&&))
import Debug.Trace
logx str x = trace (str<>" :: "<>show x) x
logf str f x = trace (str<>" :: "<>show x<>" -> _") (f x)
logfx str f x = trace (str<>" :: "<>show x<>" -> "<>show (f x)) (f x)

main = let day = "19" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 txt = sum $ sum <$> filter (workflows !> "in") parts
  where (workflows,parts) = parse1 txt

solve2 txt = undefined{-sum $ size <$> (workflows !> "in") (0,4000)
  where workflows = parse2 txt
        size r = undefined-}

type Part = [Int]
type Workflow = Part -> Bool

parse1 :: String -> ([(String, Workflow)], [Part])
parse1 = (fixAssoc . map perWorkflow . head &&& map perPart . last) . splitOn [""] . lines
  where perPart = map (read . last . splitOn "=") . splitOn "," . init
        perWorkflow str = let [name,steps'] = splitOn "{" str
          in (name, (makeWorkflow . splitOn "," . init) steps')

makeWorkflow :: [String] -> [(String, Workflow)] -> Workflow
makeWorkflow rules workflows part = foldr check (call (last rules) part) (init rules)
  where check rule rest = let
            [(f:c:ds),dest] = splitOn ":" rule
            field = part !! (zip "xmas" [0..] !> f)
            cmp = case c of '>' -> (>) ; '<' -> (<)
            num = read ds
          in if field`cmp`num then call dest part else rest
        call s = case s of
          "A" -> const True ; "R" -> const False ; w -> workflows !> w

{-type PartRange = [(Int,Int)]
type RangeWorkflow = PartRange -> PartRange

parse2 :: String -> [(String, RangeWorkflow)]
parse2 = fixAssoc . map perWorkflow . head . splitOn [""] . lines
  where perWorkflow str = let [name,steps'] = splitOn "{" str
          in (name, (makeRangeWorkflow . splitOn "," . init) steps')

makeRangeWorkflow :: [String] -> [(String, RangeWorkflow)] -> RangeWorkflow
makeRangeWorkflow rules workflows partr = foldr check (call (last rules) partr) (init rules)
  where check rule rest = let
            [(f:c:ds),dest] = splitOn ":" rule
            field = part !! (zip "xmas" [0..] !> f)
            cmp = case c of '>' -> (>) ; '<' -> (<)
            num = read ds
          in if field`cmp`num then call dest partr ++ rest partr''
        call s = case s of
          "A" -> const True ; "R" -> const False ; w -> workflows !> w-}

{-NOTE c.f.
fix :: ((a -> b) -> (a -> b)) -> (a -> b)
fix f = f (fix f)  -}
fixAssoc :: [(a, [(a,b -> c)] -> b -> c)] -> [(a,b -> c)]
fixAssoc cs = map (fmap ($ fixAssoc cs)) cs

(!>) :: (Eq a)=> [(a,b)] -> a -> b
xys !> x = maybe (error "not in assoc list") id (lookup x xys)
