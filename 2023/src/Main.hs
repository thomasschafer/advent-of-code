import Day23 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_23_test.txt"
  realData <- readFile "data/day_23.txt"
  print . (\res -> (res == 94, res)) $ part1 testData
  print . (\res -> (res == 2086, res)) $ part1 realData
  print . (\res -> (res == 154, res)) $ part2 testData
  print $ part2 realData
  return ()
