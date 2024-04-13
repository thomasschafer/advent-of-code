module Day16 (part1, part2) where

import Control.Arrow (Arrow (second))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (isPrefixOf, transpose)
import Data.List.Split (splitOn)

parseBoundaries :: String -> (String, [(Int, Int)])
parseBoundaries s = (name, boundaries)
 where
  [name, bsStr] = splitOn ": " s
  boundaries =
    map ((\[a, b] -> (a, b)) . map read . splitOn "-") $
      splitOn " or " bsStr

data Notes = Notes
  { bounds :: HashMap String [(Int, Int)]
  , myTicket :: [Int]
  , nearbyTickets :: [[Int]]
  }
  deriving (Show)

parse :: String -> Notes
parse d = Notes{bounds, myTicket, nearbyTickets}
 where
  [boundsStrs, [_, myTicketStr], (_ : nearbyTicketsStr)] = map (splitOn "\n") $ splitOn "\n\n" d
  bounds = HM.fromList $ map parseBoundaries boundsStrs
  myTicket = map read $ splitOn "," myTicketStr
  nearbyTickets = map (map read . splitOn ",") $ filter (not . null) nearbyTicketsStr

betweenBounds :: HashMap String [(Int, Int)] -> Int -> Bool
betweenBounds bounds v = any (any (\(l, h) -> v >= l && v <= h)) $ HM.elems bounds

part1 :: String -> Int
part1 s = sum $ concatMap (filter $ not . betweenBounds bounds) nearbyTickets
 where
  Notes{bounds, nearbyTickets} = parse s

solve :: (Eq a, Show a) => [[a]] -> [a]
solve = solve' . zip [0 ..]
 where
  solve' xs
    | all ((== 1) . length . snd) xs = map (head . snd) xs
    | any null xs = error $ "Found null element in " ++ show xs
    | otherwise = solve' $ map prune xs
   where
    singles = map (second head) $ filter ((== 1) . length . snd) xs
    prune cur@(idx, possibleSolns) =
      if idx `elem` map fst singles
        then cur
        else (idx, filter (`notElem` map snd singles) possibleSolns)

allValid :: (Ord a) => [(a, a)] -> [a] -> Bool
allValid bs = all $ \n -> any (\(l, h) -> l <= n && n <= h) bs

part2 :: String -> Int
part2 s = product [myTicket !! i | i <- departureIndices]
 where
  Notes{bounds, myTicket, nearbyTickets} = parse s
  validTickets = filter (all $ betweenBounds bounds) nearbyTickets
  possibleSolutions = map (\ns -> map fst . filter (flip allValid ns . snd) $ HM.toList bounds) $ transpose validTickets
  departureIndices = map fst . filter (isPrefixOf "departure" . snd) . zip [0 ..] $ solve possibleSolutions
