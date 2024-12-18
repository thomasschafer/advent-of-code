import Day18 (part1Full, part1Test, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_18_test.txt"
  realData <- readFile "data/day_18.txt"
  print $ part1Test testData
  print $ part1Full realData
  -- print $ part2 testData
  -- print $ part2 realData
  return ()
