import Data.Array
import qualified Data.Set as S

main = let day = "16" in do
  txt <- readFile (day<>".txt")
  putStrLn ("Opening Advent calendar door "<>day<>" where")
  putStrLn ("  part 1 = "<>show (solve1 txt))
  putStrLn ("  part 2 = "<>show (solve2 txt))

solve1 txt = (S.size . S.map fst . runDfs grid) ((0,0),Rt)
  where grid = parse txt

solve2 txt = maximum $ (S.size . S.map fst . runDfs grid) <$> edgeStarts
  where grid = parse txt
        ((y0,x0),(y1,x1)) = bounds grid
        edgeStarts = [((y,x0),Rt) | y<-[y0..y1]] ++ [((y1,x),Up) | x<-[x0..x1]] ++
                     [((y,x1),Lf) | y<-[y0..y1]] ++ [((y0,x),Dn) | x<-[x0..x1]]

data Dir = Rt | Up | Lf | Dn
  deriving (Eq,Ord)

runDfs :: Array (Int,Int) Char -> ((Int,Int),Dir) -> S.Set ((Int,Int),Dir)
runDfs grid start = dfs (S.singleton start) [start]
  where dfs seen []           = seen
        dfs seen (node:queue) = dfs newSeen (newNodes++queue)
          where newNodes = filter (`S.notMember`seen) (neighbors node)
                newSeen  = foldr S.insert seen newNodes
        neighbors ((y,x),dir) = filter (inRange (bounds grid) . fst) (case dir of
            Rt -> [n | (cs,n)<-[(".-",r),("/|",u),("\\|",d)], c`elem`cs]
            Up -> [n | (cs,n)<-[(".|",u),("/-",r),("\\-",l)], c`elem`cs]
            Lf -> [n | (cs,n)<-[(".-",l),("/|",d),("\\|",u)], c`elem`cs]
            Dn -> [n | (cs,n)<-[(".|",d),("/-",l),("\\-",r)], c`elem`cs] )
          where c = grid!(y,x)
                [r,u,l,d] = [((y,x+1),Rt),((y-1,x),Up),((y,x-1),Lf),((y+1,x),Dn)]

parse :: String -> Array (Int,Int) Char
parse txt = listArray ((0,0),(n,n)) (filter (/='\n') txt)
  where n = length (lines txt) - 1

{-NOTE old snippet
        neighbors ((y,x),dir) =
          let [rt,up,lf,dn] = [((y,x+1),Rt),((y-1,x),Up),((y,x-1),Lf),((y+1,x),Dn)]
              c = grid!(y,x)
          in filter (inRange (bounds grid) . fst) $ case dir of
            Rt -> if c`elem`".-" then [rt]
                  else listIf (c`elem`"/|") up ++ listIf (c`elem`"\\|") dn
            Up -> if c`elem`".|" then [up]
                  else listIf (c`elem`"/-") rt ++ listIf (c`elem`"\\-") lf
            Lf -> if c`elem`".-" then [lf]
                  else listIf (c`elem`"/|") dn ++ listIf (c`elem`"\\|") up
            Up -> if c`elem`".|" then [dn]
                  else listIf (c`elem`"/-") lf ++ listIf (c`elem`"\\-") rt

listIf :: Bool -> a -> [a]
listIf True x = [x]
listIf False _ = []
-}

{-NOTE old snippet
        neighbors ((y,x),dir) = filter (inRange (bounds grid) . fst) $
          case (grid!(y,x),dir) of
            ('.', Rt) -> [((y,x+1),Rt)]
            ('-', Rt) -> [((y,x+1),Rt)]
            ('/', Rt) -> [((y-1,x),Up)]
            ('\\',Rt) -> [             ((y+1,x),Dn)]
            ('|', Rt) -> [((y-1,x),Up),((y+1,x),Dn)]
            ('.', Up) -> [((y-1,x),Up)]
            ('|', Up) -> [((y-1,x),Up)]
            ('/', Up) -> [((y,x+1),Rt)]
            ('\\',Up) -> [             ((y,x-1),Lf)]
            ('-', Up) -> [((y,x+1),Rt),((y,x-1),Lf)]
            ('.', Lf) -> [((y,x-1),Lf)]
            ('-', Lf) -> [((y,x-1),Lf)]
            ('\\',Lf) -> [((y-1,x),Up)]
            ('/', Lf) -> [             ((y+1,x),Dn)]
            ('|', Lf) -> [((y-1,x),Up),((y+1,x),Dn)]
            ('.', Dn) -> [((y+1,x),Dn)]
            ('|', Dn) -> [((y+1,x),Dn)]
            ('\\',Dn) -> [((y,x+1),Rt)]
            ('/', Dn) -> [             ((y,x-1),Lf)]
            ('-', Dn) -> [((y,x+1),Rt),((y,x-1),Lf)]
-}
