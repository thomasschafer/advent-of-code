module Main where

import Data.Char (toUpper)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- reverse . dropWhile (== '\n') . reverse <$> getContents
  print (part1 input)

-- When you start part 2, uncomment the next line.
-- print (part2 input)

reactsWith :: Char -> Char -> Bool
reactsWith a b = toUpper a == toUpper b && a /= b

part1 :: String -> Int
part1 = length . reactAll []
  where
    reactAll stack [] = stack
    reactAll [] (l : ls) = reactAll [l] ls
    reactAll stack@(top : rest) (l : ls)
      | top `reactsWith` l = reactAll rest ls
      | otherwise = reactAll (l : stack) ls

part2 :: String -> Int
part2 = undefined
