import Day20 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_20_test.txt"
  testData2 <- readFile "data/day_20_test_2.txt"
  realData <- readFile "data/day_20.txt"
  print . (\r -> (r == 32000000, r)) $ part1 testData
  print . (\r -> (r == 11687500, r)) $ part1 testData2
  print . (\r -> (r == 898731036, r)) $ part1 realData
  -- print $ part2 testData
  -- print $ part2 testData2
  print $ part2 realData
  return ()
