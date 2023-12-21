import Day21 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_21_test.txt"
  realData <- readFile "data/day_21.txt"
  print . (\res -> (res == 16, res)) $ part1 6 testData
  print . (\res -> (res == 3646, res)) $ part1 64 realData
  -- print $ part2 testData
  -- print $ part2 realData
  return ()
