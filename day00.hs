day = undefined

main = do
  putStrLn ("Opening Advent calendar door "<>day<>"...")
  txt <- init <$> readFile (day<>".txt")
  let sol1 = undefined::Int
  putStrLn ("Part 1 contains: "<>show sol1)
  let sol2 = undefined::Int
  putStrLn ("Part 2 contains: "<>show sol2)
