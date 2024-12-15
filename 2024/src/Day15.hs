module Day15 (part1, part2) where

import Control.Arrow (Arrow (first, second))
import Data.Bifunctor (bimap)
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Utils (toTuple)

data Obj = Robot | Box | Wall deriving (Eq, Show)

type Grid = [[Maybe Obj]]

parseGrid :: [String] -> Grid
parseGrid = map . map $ \case
  '#' -> Just Wall
  'O' -> Just Box
  '@' -> Just Robot
  '.' -> Nothing
  c -> error $ "Unexpected grid char " ++ show c

data Move = U | D | L | R deriving (Show)

parseMoves :: String -> [Move]
parseMoves = mapMaybe $ \case
  '<' -> Just L
  '>' -> Just R
  '^' -> Just U
  'v' -> Just D
  '\n' -> Nothing
  c -> error $ "Unexpected move char " ++ show c

positionsOf :: Grid -> Maybe Obj -> [(Int, Int)]
positionsOf grid x =
  [ (r, c)
    | r <- [0 .. length grid - 1],
      c <- [0 .. length (head grid) - 1],
      grid !! r !! c == x
  ]

updateMultiple2d :: [((Int, Int), a)] -> [[a]] -> [[a]]
updateMultiple2d updates grid =
  [ [ case snd <$> find ((== (r, c)) . fst) updates of
        Just x -> x
        Nothing -> grid !! r !! c
      | c <- [0 .. length (head grid) - 1]
    ]
    | r <- [0 .. length grid - 1]
  ]

applyMove :: Move -> Grid -> Grid
applyMove move grid = case movesToFreeSq of
  Nothing -> grid
  Just moves -> flip updateMultiple2d grid . ((robotPos, Nothing) :) $ case move of
    U -> [(first (subtract 1) robotPos, Just Robot), (first (subtract moves) robotPos, Just Box)]
    D -> [(first (+ 1) robotPos, Just Robot), (first (+ moves) robotPos, Just Box)]
    L -> [(second (subtract 1) robotPos, Just Robot), (second (subtract moves) robotPos, Just Box)]
    R -> [(second (+ 1) robotPos, Just Robot), (second (+ moves) robotPos, Just Box)]
  where
    robotPos@(robotRow, robotCol) = head $ positionsOf grid (Just Robot)
    view = takeWhile (/= Just Wall) $ case move of
      U -> reverse $ map (!! robotCol) $ take robotRow grid
      D -> map (!! robotCol) $ drop (robotRow + 1) grid
      L -> reverse $ take robotCol (grid !! robotRow)
      R -> drop (robotCol + 1) (grid !! robotRow)
    movesToFreeSq = (+ 1) <$> elemIndex Nothing view

gpsCoords :: Grid -> Int
gpsCoords grid = sum . map (uncurry (+) . first (100 *)) $ positionsOf grid (Just Box)

part1 :: String -> Int
part1 = gpsCoords . uncurry (foldl $ flip applyMove) . bimap (parseGrid . lines) parseMoves . toTuple . splitOn "\n\n"

part2 :: String -> Int
part2 = const 2
