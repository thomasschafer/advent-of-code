import Day09 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_9_test.txt"
  realData <- readFile "data/day_9.txt"
  print $ part1 5 testData
  print $ part1 25 realData
  print $ part2 testData
  print $ part2 realData
