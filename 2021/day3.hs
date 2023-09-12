import Data.List (group, sort)

binaryStringToInt :: String -> Int
binaryStringToInt = foldl (\acc x -> acc * 2 + read [x]) 0

charCounts :: Ord b => [b] -> [(Int, b)]
charCounts ns = [(length ks, head ks) | ks <- group (sort ns)]

groupChars :: [String] -> [[(Int, Char)]]
groupChars xs =
  let nonEmptyStrs = filter (not . null) xs
   in if null nonEmptyStrs
        then []
        else charCounts (map head nonEmptyStrs) : groupChars (map tail nonEmptyStrs)

solvePart1 :: [String] -> Int
solvePart1 diagnosticData = do
  gammaRate * epsilonRate
  where
    groupedChars = groupChars diagnosticData
    mostFrequentChars = map (snd . maximum) groupedChars
    gammaRate = binaryStringToInt mostFrequentChars
    leastFrequentChars = map (\x -> if x == '1' then '0' else '1') mostFrequentChars
    epsilonRate = binaryStringToInt leastFrequentChars

main :: IO ()
main = do
  diagnosticData <- readFile "data/day_3.txt"
  print $ solvePart1 $ lines diagnosticData
