import Day14 (part1Full, part1Test, part2Full, part2Test)

main :: IO ()
main = do
  testData <- readFile "data/day_14_test.txt"
  realData <- readFile "data/day_14.txt"
  print $ part1Test testData
  print $ part1Full realData
  part2Test testData
  part2Full realData
