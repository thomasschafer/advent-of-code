import Day11 (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_11_test.txt"
  realData <- readFile "data/day_11.txt"
  print $ (\ans -> (ans == 374, ans)) $ part1 testData
  print $ (\ans -> (ans == 9684228, ans)) $ part1 realData
  print $ (\ans -> (ans == 82000210, ans)) $ part2 testData
  print $ (\ans -> (ans == 483844716556, ans)) $ part2 realData
  return ()
