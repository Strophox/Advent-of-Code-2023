import Data.List
import Data.List.Split (splitOn)
import Control.Arrow ((***),(&&&))
import Control.Monad.ST (ST, runST)
import qualified Data.HashTable.Class as H
import Data.HashTable.ST.Cuckoo (HashTable)

import Debug.Trace
logx str x = trace (str<>" :: "<>show x) x
logf str f x = trace (str<>" :: "<>show x<>" -> _") (f x)
logfx str f x = trace (str<>" :: "<>show x<>" -> "<>show (f x)) (f x)

main = let day = "20" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 = countPulses . logx "hello" . parse where
  countPulses :: [(String, (ModuleType,[String]))] -> Int
  countPulses modules = runST $ do
    modStates <- H.fromList modules
    return (logx "hi" "hi")
    pulses <- mapM (const $ run modStates [(False,"button","broadcaster")] (0,0)) [1..1000]
    return (sum (map fst pulses) * sum (map snd pulses))
  run :: HashTable s String (ModuleType,[String]) -> [(Bool,String,String)] -> (Int,Int) -> ST s (Int,Int)
  run _ [] (n,m) = return (n,m)
  run modStates ((pulse,src,curr):queue) (i,j) = do
    let newIJ = if not pulse then (i+1,j) else (i,j+1)
    modState <- H.lookup modStates curr
    case modState of
      Nothing -> run modStates queue newIJ
      Just (ty,dests) -> case ty of
        FlipFlop state -> do
          if pulse then
            run modStates queue newIJ
          else do
            let newstate = not state
            H.mutate modStates curr (\m -> case m of
              Just (FlipFlop _,dests) -> (Just (FlipFlop newstate,dests), ())
              x -> (x, ()))
            let newpulse = not state
            run modStates (queue ++ map (\dst -> (newpulse,curr,dst)) dests) newIJ
        Conjunction states -> do
          let newstates = update src pulse states
          H.mutate modStates curr (\m -> case m of
            Just (FlipFlop _,dests) -> (Just (Conjunction newstates,dests), ())
            x -> (x, ()))
          let newpulse = any (not.snd) newstates
          run modStates (queue ++ map (\dst -> (newpulse,curr,dst)) dests) newIJ
        Broadcaster -> do
          run modStates (queue ++ map (\dst -> (pulse,curr,dst)) dests) newIJ

solve2 = const "todo"

update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update _ _ [] = []
update a b ((c,d):rest)
  | a == c    = (c,b):rest
  | otherwise = (c,d) : update a b rest

data ModuleType = FlipFlop Bool | Conjunction [(String,Bool)] | Broadcaster
  deriving (Show)

parse :: String -> [(String, (ModuleType,[String]))]
parse = (\mods -> map (fillConjunction mods) mods) . map perLine . lines
  where perLine :: String -> (String, (ModuleType,[String]))
        perLine = (\((name,ty),dests) -> (name,(ty,dests))) . (parseModule . head &&& splitOn ", " . last) . splitOn " -> "
        parseModule :: String -> (String, ModuleType)
        parseModule (c:cs) = case c of
          '%' -> (cs, FlipFlop False)
          '&' -> (cs, Conjunction [])
          _ -> (c:cs, Broadcaster)
        fillConjunction :: [(String, (ModuleType,[String]))] -> (String, (ModuleType,[String])) -> (String, (ModuleType,[String]))
        fillConjunction allModules (name,(Conjunction [],dests)) = let
          srcs = [(n,False) | (n,(_,ds)) <- allModules, name`elem`ds]
          in (name,(Conjunction srcs,dests))
        fillConjunction _ m = m

{-

-- flip-flop '%'
True  + F b     -> F b
False + F False -> F True + True
False + F True  -> F False + False

-- conjunction '&'
b' + C bs -> C{ bs with b=b' } + (not.and) {bs with b=b'}

-- 1 broadcast "broadcaster"
b + B = B + b

-- 1 button
() = False

-}
