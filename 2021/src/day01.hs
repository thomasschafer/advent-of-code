module Day1 (day1Main) where

numIncreasesPt1 :: [Int] -> Int
numIncreasesPt1 depths =
  snd $ foldl incrIfGreater (Nothing, 0) depths
  where
    incrIfGreater (Nothing, c) next = (Just next, c)
    incrIfGreater (Just prev, c) next = (Just next, c + (if next > prev then 1 else 0))

numIncreasesPt2 :: Int -> [Int] -> Int
numIncreasesPt2 windowLength depths =
  snd $ foldl threeSum (Nothing, 0) [0 .. (length depths - windowLength)]
  where
    threeSum (prev, c) idx = case prev of
      Nothing -> (Just nextSum, c)
      (Just prevSum) -> (Just nextSum, c + (if nextSum > prevSum then 1 else 0))
      where
        nextSum = sum $ take windowLength $ drop idx depths

day1Main :: IO ()
day1Main = do
  depthsData <- readFile "data/day_1.txt"
  let depths = map read $ lines depthsData
  print $ numIncreasesPt1 depths
  print $ numIncreasesPt2 3 depths
