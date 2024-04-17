import Day18 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_18_test.txt"
  realData <- readFile "data/day_18.txt"
  print $ part1 testData -- 51 + 26 + 437 + 12240 + 13632 = 26386
  print $ part1 realData
  print $ part2 testData -- 51 + 46 + 1445 + 669060 + 23340 = 693942
  print $ part2 realData
