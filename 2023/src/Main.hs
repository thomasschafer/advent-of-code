import Day19 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_19_test.txt"
  realData <- readFile "data/day_19.txt"
  print $ (\r -> (r == 19114, r)) $ part1 testData
  print $ (\r -> (r == 432788, r)) $ part1 realData
  print $ (\r -> (r == 167409079868000, r)) $ part2 testData
  print $ (\r -> (r == 142863718918201, r)) $ part2 realData
  return ()
