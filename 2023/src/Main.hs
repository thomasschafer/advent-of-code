import Day18 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_17_test.txt"
  testData3 <- readFile "data/day_17_test_3.txt"
  realData <- readFile "data/day_17.txt"
  print $ part1 testData -- 102
  -- print $ part1 realData
  print $ part2 testData -- 94
  print $ part2 testData3 -- 71
  -- print $ part2 realData
  return ()
