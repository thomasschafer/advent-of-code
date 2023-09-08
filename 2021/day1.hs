numIncreases :: [Int] -> Int
numIncreases depths =
  snd $ foldl incrIfGreater (Nothing, 0) depths
  where
    incrIfGreater (Nothing, c) next = (Just next, c)
    incrIfGreater (Just prev, c) next = (Just next, c + (if next > prev then 1 else 0))

main :: IO ()
main = do
  depthsData <- readFile "data/day_1.txt"
  let depths = map read $ lines depthsData
  print $ numIncreases depths
