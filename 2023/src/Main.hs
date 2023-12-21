import Day21 (part1, part2, solve)

main :: IO ()
main = do
  testData <- readFile "data/day_21_test.txt"
  realData <- readFile "data/day_21.txt"
  print . (\res -> (res == 16, res)) $ solve False 6 testData
  print . (\res -> (res == 3646, res)) $ part1 realData
  print . (\res -> (res == 16, res)) $ solve True 6 testData
  print . (\res -> (res == 50, res)) $ solve True 10 testData
  print . (\res -> (res == 1594, res)) $ solve True 50 testData
  print . (\res -> (res == 6536, res)) $ solve True 100 testData
  print . (\res -> (res == 167004, res)) $ solve True 500 testData
  print . (\res -> (res == 668697, res)) $ solve True 1000 testData
  print . (\res -> (res == 16733044, res)) $ solve True 5000 testData
  -- print $ part2 realData
  return ()
