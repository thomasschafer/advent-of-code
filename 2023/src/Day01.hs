module Day01 (part1, part2) where

part1 :: String -> Int
part1 = sum . map (read . (\v -> [head v, last v]) . filter (`elem` ['1' .. '9'])) . lines

findValidDigitFromStart :: (String -> Int) -> [String] -> String -> Maybe Int
findValidDigitFromStart wordToInt validDigits = findValidDigitFromStart' validDigits ""
  where
    findValidDigitFromStart' [] _ _ = Nothing
    findValidDigitFromStart' valid start (nextDigit : rest)
      | [nextDigit] `elem` filter ((== 1) . length) valid =
          Just . wordToInt $ (start ++ [nextDigit])
      | nextDigit `elem` map head valid =
          findValidDigitFromStart' (map tail $ filter ((== nextDigit) . head) valid) (start ++ [nextDigit]) rest
      | otherwise = Nothing

findValidDigit :: (String -> Int) -> [String] -> String -> Int
findValidDigit wordToInt valid s =
  case findValidDigitFromStart wordToInt valid s of
    Just x -> x
    Nothing -> findValidDigit wordToInt valid (tail s)

validDigits :: [String]
validDigits = map show [1 .. 9] ++ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

firstWordToInt :: String -> Int
firstWordToInt "one" = 1
firstWordToInt "two" = 2
firstWordToInt "three" = 3
firstWordToInt "four" = 4
firstWordToInt "five" = 5
firstWordToInt "six" = 6
firstWordToInt "seven" = 7
firstWordToInt "eight" = 8
firstWordToInt "nine" = 9
firstWordToInt s = read s

part2 :: String -> Int
part2 calibrationData = sum . map firstAndLastDigit $ lines calibrationData
  where
    findFirstValidDigit = findValidDigit firstWordToInt validDigits
    findLastValidDigit s = findValidDigit (firstWordToInt . reverse) (map reverse validDigits) (reverse s)

    firstAndLastDigit s = read $ concatMap (\f -> (show . f) s) [findFirstValidDigit, findLastValidDigit]
