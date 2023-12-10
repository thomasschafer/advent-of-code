import Day10 (part1, part2)

main :: IO ()
main = do
  testData_1_1 <- readFile "data/day_10_test_1_1.txt"
  testData_1_2 <- readFile "data/day_10_test_1_2.txt"
  testData_2_1 <- readFile "data/day_10_test_2_1.txt"
  testData_2_2 <- readFile "data/day_10_test_2_2.txt"
  testData_2_3 <- readFile "data/day_10_test_2_3.txt"
  testData_2_4 <- readFile "data/day_10_test_2_4.txt"
  realData <- readFile "data/day_10.txt"
  print . (\ans -> (ans == 4, ans)) $ part1 testData_1_1
  print . (\ans -> (ans == 8, ans)) $ part1 testData_1_2
  print . (\ans -> (ans == 6738, ans)) $ part1 realData
  print . (\ans -> (ans == 1, ans)) $ part2 testData_1_1
  print . (\ans -> (ans == 1, ans)) $ part2 testData_1_2
  print . (\ans -> (ans == 4, ans)) $ part2 testData_2_1
  print . (\ans -> (ans == 4, ans)) $ part2 testData_2_2
  print . (\ans -> (ans == 8, ans)) $ part2 testData_2_3
  print . (\ans -> (ans == 10, ans)) $ part2 testData_2_4
  print . (\ans -> (ans == 579, ans)) $ part2 realData
  return ()
