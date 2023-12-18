import Day18 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_18_test.txt"
  realData <- readFile "data/day_18.txt"
  print $ (\r -> (r == 62, r)) $ part1 testData -- 62
  print $ (\r -> (r == 40745, r)) $ part1 realData -- 40745
  print $ (\r -> (r == 952408144115, r)) $ part2 testData -- 952408144115
  print $ part2 realData
  return ()
