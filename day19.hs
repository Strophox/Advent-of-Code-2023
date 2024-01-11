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

solve1 txt = sum . map rating . filter (workflows!>"in") $ parts
  where (workflows,parts) = parse1 txt
        rating = sum

solve2 txt = sum . map size . (rangeWorkflows!>"in") $ replicate 4 (0,4000)
  where rangeWorkflows = parse2 txt
        size = product . map (\(a,b) -> b-a+1)

type Part = [Int]
type Workflow = Part -> Bool

parse1 :: String -> ([(String, Workflow)], [Part])
parse1 = (fixAssoc . map perWorkflow . head &&& map perPart . last) . splitOn [""] . lines
  where perPart = map (read . last . splitOn "=") . splitOn "," . init
        perWorkflow str = let [name,steps'] = splitOn "{" str
          in (name, (buildWorkflow . splitOn "," . init) steps')

buildWorkflow :: [String] -> [(String, Workflow)] -> Workflow
buildWorkflow rules workflows part = foldr check (call (last rules) part) (init rules)
  where check :: String -> Bool -> Bool
        check rule next = let
          [f:c:num,fn] = splitOn ":" rule
          field = zip "xmas" part !> f
          cmp = case c of '>' -> (>); '<' -> (<)
          n = read num
          in if field`cmp`n then call fn part else next
        call fn = case fn of "A" -> pure True; "R" -> pure False; str -> workflows!>str

type PartRange = [(Int,Int)]
type RangeWorkflow = PartRange -> [PartRange]

parse2 :: String -> [(String, RangeWorkflow)]
parse2 = fixAssoc . map perWorkflow . takeWhile (not.null) . lines
  where perWorkflow str = let [name,steps'] = splitOn "{" str
          in (name, (buildRangeWorkflow . splitOn "," . init) steps')

buildRangeWorkflow :: [String] -> [(String, RangeWorkflow)] -> RangeWorkflow
buildRangeWorkflow rules workflows partRng = foldr branch (call (last rules)) (init rules) partRng
  where branch :: String -> (PartRange -> [PartRange]) -> PartRange -> [PartRange]
        branch rule next partRng = if any (\(l,r) -> r < l) partRng then [] else let
          [f:c:num,fn] = splitOn ":" rule
          n = read num
          (thenRng,elseRng) = let
            (as,(l,r):cs) = splitAt (zip "xmas" [0..] !> f) partRng
            (b1,b0) = case c of '>' -> ((n+1,r),(l,n)); '<' -> ((l,n-1),(n,r))
            in (as++b1:cs, as++b0:cs)
          in call fn thenRng ++ next elseRng {-FIXME-}
        call fn = case fn of "A" -> (:[]); "R" -> const []; str -> workflows!>str

infixl 9 !>
(!>) :: (Eq a)=> [(a,b)] -> a -> b
xys !> x = maybe (error "not in assoc list") id (lookup x xys)

fixAssoc :: [(a, [(a, b -> c)] -> b -> c)] -> [(a, b -> c)]
fixAssoc cs = map (fmap ($ fixAssoc cs)) cs
{-NOTE c.f.
fix :: ((a -> b) -> (a -> b)) -> (a -> b)
fix f = f (fix f)-}
