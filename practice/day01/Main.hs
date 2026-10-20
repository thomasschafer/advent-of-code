module Main where

import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  -- When you start part 2, uncomment the next line.
  -- print (part2 input)

part1 :: String -> Int
part1 = length
  . filter (> 100)
  . map (sum . map read . words)
  . lines

part2 :: String -> Int
part2 = const 2
