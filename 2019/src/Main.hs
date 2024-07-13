import Day05 (part1, part1_day2_real, part1_day2_test, part2)

main :: IO ()
main = do
  testDataDay2 <- readFile "data/day_02_test.txt"
  realDataDay2 <- readFile "data/day_02.txt"
  testData <- readFile "data/day_05_test.txt"
  realData <- readFile "data/day_05.txt"
  print $ part1_day2_test testDataDay2 -- 3500
  print $ part1_day2_real realDataDay2 -- 5866663
  -- print $ part1 testData
  print $ part1 realData
  -- print $ part2 testData
  -- print $ part2 realData
  return ()
