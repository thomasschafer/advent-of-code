import Day01 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_01_test.txt"
  realData <- readFile "data/day_01.txt"
  print $ part1 testData
  -- print $ part1 realData
  -- print $ part2 testData
  -- print $ part2 realData
