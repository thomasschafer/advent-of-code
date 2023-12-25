import Day24 (part1Real, part1Test, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_24_test.txt"
  realData <- readFile "data/day_24.txt"
  print $ part1Test testData
  print $ part1Real realData
  -- print $ part2 testData
  -- print $ part2 realData
  return ()
