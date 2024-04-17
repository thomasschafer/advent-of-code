import Day18 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_18_test.txt"
  realData <- readFile "data/day_18.txt"
  print $ part1 testData -- 71 + 26 + 437 + 12240 + 13632 = 26406
  print $ part1 realData
  -- print $ part2 testData
  -- print $ part2 realData
  return ()
