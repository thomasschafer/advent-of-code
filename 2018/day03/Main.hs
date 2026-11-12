module Main where

import Data.Bifunctor (second)
import Data.List.Split (splitOn)
import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe) 
import Data.Set (Set)
import qualified Data.Set as S
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Debug.Trace (traceShowId, trace)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  -- When you start part 2, uncomment the next line.
  -- print (part2 input)

data Claim = Claim {
  cid :: Int,
  xStart :: Int,
  yStart :: Int,
  width :: Int,
  height :: Int
}
  deriving (Show)

parseClaim :: String -> Claim
parseClaim s = Claim { cid, xStart, yStart, width, height }
  where
    [idStr, _, startStr, dimStr] = words s
    cid = read $ tail idStr
    [xStart, yStart] = map read . splitOn "," $ init startStr
    [width, height] = map read $ splitOn "x" dimStr 

data Boundary = BLeft | BRight | BTop | BBottom
  deriving (Eq, Ord, Show)

buildBoundaryMap :: [Claim] -> HashMap (Int, Int) [Boundary]
buildBoundaryMap = HM.fromListWith (++) . map (second (:[])) . foldl insert []
  where
    insert :: [((Int, Int), Boundary)] -> Claim -> [((Int, Int), Boundary)]
    insert h c = left ++ right ++ h
      where
        left = map (,BLeft) $ map (xStart c,) [(yStart c)..(yStart c + height c - 1)]
        right = map (,BRight) $ map (xStart c + width c - 1,) [(yStart c)..(yStart c + height c - 1)]

numSharedSquares :: [((Int, Int), [Boundary])] -> Int
numSharedSquares h = sum . map numIntersections $ S.toList yPositions
  where
    yPositions = S.fromList . map snd $ map fst h
    
    numIntersections :: Int -> Int
    numIntersections y = go 0 0 xPositions
      where
        xPositions = sort $ mapMaybe (\((x, y'), b) -> if y' == y then Just (x, b) else Nothing) h

        go acc _ [] = acc
        go acc numInside ((x, bs):rest) =
            go
              (acc + fromEnum (numInside + length (filter (== BLeft) bs) > 1) + (x' - x - 1) * fromEnum (numInside' > 1))
              numInside'
              rest
          where
            numInside' = numInside + sum (map (\b -> if b == BLeft then 1 else if b == BRight then -1 else 0) bs)
            x' = fromMaybe (x+1) . fmap fst $ listToMaybe rest

part1 :: String -> Int
part1 = numSharedSquares . HM.toList . buildBoundaryMap . map parseClaim . lines

part2 :: String -> Int
part2 = undefined
