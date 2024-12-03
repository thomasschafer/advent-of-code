import Day03 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_03_test.txt"
  realData <- readFile "data/day_03.txt"
  print $ part1 testData
  print $ part1 realData
  testData2 <- readFile "data/day_03_test_2.txt"
  print $ part2 testData2
  print $ part2 realData
