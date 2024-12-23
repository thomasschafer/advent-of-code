import Day22 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_22_test.txt"
  testData2 <- readFile "data/day_22_test_2.txt"
  realData <- readFile "data/day_22.txt"
  print $ part1 testData
  print $ part1 realData
  print $ part2 testData2
  print $ part2 realData
