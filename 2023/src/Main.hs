import Day12 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_12_test.txt"
  realData <- readFile "data/day_12.txt"
  print $ part1 testData -- 21
  print $ part1 realData
  print $ part2 testData -- 525152
  print $ part2 realData
