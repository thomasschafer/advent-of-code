import Day25 (part1)

main :: IO ()
main = do
  testData <- readFile "data/day_25_test.txt"
  realData <- readFile "data/day_25.txt"
  print $ part1 testData
  print $ part1 realData
