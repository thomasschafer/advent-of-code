module Day09 (part1, part2) where

import Data.List.Split (splitOn)
import Utils (toTuple)

type Pos = (Int, Int)

parse :: String -> [Pos]
parse = map (toTuple . map read . splitOn ",") . lines

withAreas :: [Pos] -> [(Int, (Pos, Pos))]
withAreas xs =
  [ ((abs (x2 - x1) + 1) * (abs (y2 - y1) + 1), (p1, p2))
  | i <- [0 .. length xs - 2],
    j <- [(i + 1) .. length xs - 1],
    let p1@(x1, y1) = xs !! i,
    let p2@(x2, y2) = xs !! j
  ]

part1 :: String -> Int
part1 = maximum . map fst . withAreas . parse

-- data Colour = Green | Red

-- fillGreenBoundaries :: [Pos] -> HashMap Pos Colour
-- fillGreenBoundaries ps = foldl update HM.empty $ zip ps (tail ps)
--   where
--     update colourMap ((p1@(x1, y1)), (p2@(x2, y2))) =
--       foldl (flip $ uncurry HM.insert) colourMap $
--         [(p1, Red), (p2, Red)] ++ map (\p -> (p, Green)) greens
--       where
--         greens
--           | x1 == x2 = [(x1, y) | y <- [min y1 y2 + 1 .. max y1 y2 - 1]]
--           | y1 == y2 = [(x, y1) | x <- [min x1 x2 + 1 .. max x1 x2 - 1]]

-- fillGreenInner :: HashMap Pos Colour -> HashMap Pos Colour
-- fillGreenInner = id
--
-- part2 = const 1 . fillGreenInner . fillGreenBoundaries . parse

-- data VerticalLine = VerticalLine {x :: Int, y1 :: Int, y2 :: Int}

-- data HorizontalLine = HorizontalLine {y :: Int, x1 :: Int, x2 :: Int}

-- part2 :: String -> Int
-- part2 s = fst . head $ filter isValid candidates
--   where
--     corners = parse s
--     candidates = sortBy (comparing Down) $ withAreas corners
--     (vertical, horizontal) = foldl update ([], []) $ zip (last corners : init corners) corners
--       where
--         update :: ([VerticalLine], [HorizontalLine]) -> (Pos, Pos) -> ([VerticalLine], [HorizontalLine])
--         update (v, h) ((x1, y1), (x2, y2))
--           | x1 == x2 = (VerticalLine {x = x1, y1, y2} : v, h)
--           | y1 == y2 = (v, HorizontalLine {y = y1, x1, x2} : h)

part2 :: String -> Int
part2 = const 2
