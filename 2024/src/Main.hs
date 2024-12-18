import Day18 (part1Full, part1Test, part2Full, part2Test)

main :: IO ()
main = do
  testData <- readFile "data/day_18_test.txt"
  realData <- readFile "data/day_18.txt"
  print $ part1Test testData
  print $ part1Full realData
  print $ part2Test testData
  print $ part2Full realData
