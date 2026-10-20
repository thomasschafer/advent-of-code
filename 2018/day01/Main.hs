module Main where

import qualified Data.Set as S

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

parse :: String -> Int
parse (sign:num) = (if sign == '-' then -1 else 1) * read num

part1 :: String -> Int
part1 = sum . map parse . lines


part2 :: String -> Int
part2 = solve 0 S.empty . cycle . map parse . lines
  where
    solve freq seen (nxt:rest)
      | freq `S.member` seen = freq
      | otherwise = solve (freq+nxt) (S.insert freq seen) rest
