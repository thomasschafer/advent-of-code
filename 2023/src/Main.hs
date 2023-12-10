import Day10 (part1, part2)

main :: IO ()
main = do
  testData_1_1 <- readFile "data/day_10_test_1_1.txt"
  testData_1_2 <- readFile "data/day_10_test_1_2.txt"
  testData_2_1 <- readFile "data/day_10_test_1_1.txt"
  testData_2_2 <- readFile "data/day_10_test_1_2.txt"
  realData <- readFile "data/day_10.txt"
  print $ part1 testData_1_1 -- 4
  print $ part1 testData_1_2 -- 8
  print $ part1 realData
  print $ part2 testData_2_1 -- 4
  print $ part2 testData_2_2 -- 8
  -- print $ part2 realData
  return ()
