import Day08 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_8_test.txt"
  testData2 <- readFile "data/day_8_test_2.txt"
  testData3 <- readFile "data/day_8_test_3.txt"
  realData <- readFile "data/day_8.txt"
  print $ part1 testData
  print $ part1 testData2
  print $ part1 realData
  print $ part2 testData3
  print $ part2 realData