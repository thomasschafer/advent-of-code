module Day07 (day07Main) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (maximumBy, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import GHC.Exts (Down (Down))
import Utils (withIdx)

data CardHand = CardHand {cards :: [Int], bid :: Int}
  deriving (Eq, Show)

freqCounts :: [Int] -> HashMap Int Int
freqCounts = foldl update HM.empty
  where
    update countsMap n = HM.insert n (1 + fromMaybe 0 (HM.lookup n countsMap)) countsMap

toCardInt1 :: Char -> Int
toCardInt1 c = case c of
  'A' -> 14
  'K' -> 13
  'Q' -> 12
  'J' -> 11
  'T' -> 10
  n -> read [n]

parseHand :: (Char -> Int) -> String -> CardHand
parseHand toCardInt c = CardHand {cards = map toCardInt cardsStr, bid = read bidStr}
  where
    [cardsStr, bidStr] = words c

countsToType :: [Int] -> Int
countsToType [5] = 7
countsToType [4, 1] = 6
countsToType [3, 2] = 5
countsToType [3, 1, 1] = 4
countsToType [2, 2, 1] = 3
countsToType [2, 1, 1, 1] = 2
countsToType [1, 1, 1, 1, 1] = 1
countsToType cs = error $ "Error: unexpected counts ++ " ++ show cs

-- 7 for 'Five of a kind', ...,  1 for 'High card'
handType :: CardHand -> Int
handType = countsToType . sortBy (comparing Down) . HM.elems . freqCounts . cards

compareCards :: CardHand -> CardHand -> Ordering
compareCards c1 c2
  | c1 == c2 = EQ
  | (handType c1 > handType c2) || (handType c1 == handType c2 && cards c1 > cards c2) = GT
  | otherwise = LT

score :: [CardHand] -> Int
score = sum . map (\(idx, c) -> (idx + 1) * bid c) . withIdx

part1 :: String -> Int
part1 = score . sortBy compareCards . map (parseHand toCardInt1) . lines

allPossibleHands :: CardHand -> [CardHand]
allPossibleHands CardHand {cards, bid} = map (\c -> CardHand {cards = c, bid}) (cardVariants cards)
  where
    cardVariants (1 : cs) = [c : rest | c <- [2 .. 14], rest <- cardVariants cs]
    cardVariants (c : cs) = map (c :) (cardVariants cs)
    cardVariants [] = [[]]

data CardHandPair = CardHandPair {best :: CardHand, actual :: CardHand}

toBestHandPair :: CardHand -> CardHandPair
toBestHandPair c = CardHandPair {best = (maximumBy compareCards . allPossibleHands) c, actual = c}

compareCardPair :: CardHandPair -> CardHandPair -> Ordering
compareCardPair c1 c2
  | actual c1 == actual c2 = EQ
  | (handType (best c1) > handType (best c2))
      || (handType (best c1) == handType (best c2) && cards (actual c1) > cards (actual c2)) =
      GT
  | otherwise = LT

toCardInt2 :: Char -> Int
toCardInt2 'J' = 1
toCardInt2 c = toCardInt1 c

part2 :: String -> Int
part2 = score . map actual . sortBy compareCardPair . map (toBestHandPair . parseHand toCardInt2) . lines

day07Main :: IO ()
day07Main = do
  testData <- readFile "data/day_7_test.txt"
  realData <- readFile "data/day_7.txt"
  print $ part1 testData
  print $ part1 realData
  print $ part2 testData
  print $ part2 realData
