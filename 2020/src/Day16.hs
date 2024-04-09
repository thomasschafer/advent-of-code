module Day16 (part1, part2) where

import Data.List.Split (splitOn)

parseBoundaries :: String -> [(Int, Int)]
parseBoundaries =
  map ((\[a, b] -> (a, b)) . map read . splitOn "-")
    . splitOn " or "
    . last
    . splitOn ": "

data Notes = Notes
  { bounds :: [[(Int, Int)]],
    myTicket :: [Int],
    nearbyTickets :: [[Int]]
  }

parse :: String -> Notes
parse d = Notes {bounds, myTicket, nearbyTickets}
  where
    [boundsStrs, [_, myTicketStr], (_ : nearbyTicketsStr)] = map (splitOn "\n") $ splitOn "\n\n" d
    bounds = map parseBoundaries boundsStrs
    myTicket = map read $ splitOn "," myTicketStr
    nearbyTickets = map (map read . filter (not . null) . splitOn ",") nearbyTicketsStr

part1 :: String -> Int
part1 s = sum $ concatMap (filter $ not . betweenBounds) nearbyTickets
  where
    Notes {bounds, nearbyTickets} = parse s
    betweenBounds v = any (any (\(l, h) -> v >= l && v <= h)) bounds

part2 :: String -> Int
part2 = const 1
