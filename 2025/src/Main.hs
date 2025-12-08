import Day08 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_08_test.txt"
  realData <- readFile "data/day_08.txt"
  print $ part1 10 testData
  print $ part1 1000 realData
  print $ part2 testData
  print $ part2 realData
