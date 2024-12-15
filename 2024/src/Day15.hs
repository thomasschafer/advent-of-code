module Day15 (part1, part2) where

import Control.Arrow (Arrow (first))
import Control.Monad ((<=<))
import Data.Bifunctor (bimap)
import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Utils (mapTuple, toTuple)

data Obj = Robot | Box | BoxLeft | BoxRight | Wall deriving (Eq, Show)

type Grid = [[Maybe Obj]]

data Move = U | D | L | R deriving (Eq, Show)

positionsOf :: Grid -> [Maybe Obj] -> [(Int, Int)]
positionsOf grid xs =
  [ (r, c)
    | r <- [0 .. length grid - 1],
      c <- [0 .. length (head grid) - 1],
      grid !! r !! c `elem` xs
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
applyMove move grid = case move of
  U -> fromMaybe grid $ shiftAllVertical (-1) robotPos grid
  D -> fromMaybe grid $ shiftAllVertical 1 robotPos grid
  L -> fromMaybe grid $ shiftAllHorizontal (-1) robotPos grid
  R -> fromMaybe grid $ shiftAllHorizontal 1 robotPos grid
  where
    robotPos = head $ positionsOf grid [Just Robot]

shiftAllVertical :: Int -> (Int, Int) -> Grid -> Maybe Grid
shiftAllVertical step (r, c) grid = case grid !! (r + step) !! c of
  Nothing -> Just $ moveCur grid
  Just Wall -> Nothing
  Just Box -> moveCur <$> shiftAllVertical step (r + step, c) grid
  Just BoxLeft -> moveCur <$> (shiftAllVertical step (r + step, c + 1) <=< shiftAllVertical step (r + step, c)) grid
  Just BoxRight -> moveCur <$> (shiftAllVertical step (r + step, c - 1) <=< shiftAllVertical step (r + step, c)) grid
  where
    moveCur = updateMultiple2d [((r, c), Nothing), ((r + step, c), grid !! r !! c)]

shiftAllHorizontal :: Int -> (Int, Int) -> Grid -> Maybe Grid
shiftAllHorizontal step (r, c) grid = case grid !! r !! (c + step) of
  Nothing -> Just $ moveCur grid
  Just Wall -> Nothing
  Just b | b `elem` [Box, BoxLeft, BoxRight] -> moveCur <$> shiftAllHorizontal step (r, c + step) grid
  where
    moveCur = updateMultiple2d [((r, c), Nothing), ((r, c + step), grid !! r !! c)]

gpsCoords :: Grid -> Int
gpsCoords grid = sum . map (uncurry (+) . first (100 *)) . positionsOf grid $ map Just [Box, BoxLeft]

parse :: String -> (Grid, [Move])
parse = bimap (parseGrid . lines) parseMoves . toTuple . splitOn "\n\n"
  where
    parseGrid = map . map $ \case
      '#' -> Just Wall
      'O' -> Just Box
      '@' -> Just Robot
      '.' -> Nothing

    parseMoves = mapMaybe $ \case
      '<' -> Just L
      '>' -> Just R
      '^' -> Just U
      'v' -> Just D
      '\n' -> Nothing

solve :: Bool -> String -> Int
solve expandGrid =
  gpsCoords
    . uncurry (foldl $ flip applyMove)
    . (if expandGrid then first expand else id)
    . parse

expand :: Grid -> Grid
expand = map (concatMap expand')
  where
    expand' (Just Robot) = [Just Robot, Nothing]
    expand' (Just Box) = [Just BoxLeft, Just BoxRight]
    expand' x = [x, x]

part1, part2 :: String -> Int
(part1, part2) = mapTuple solve (False, True)
