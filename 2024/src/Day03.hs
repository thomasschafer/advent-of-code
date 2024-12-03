module Day03 (part1, part2) where

import Data.Char (isDigit)
import Utils (safeTail)

type Parser a = String -> Either String (a, String)

consumeStr :: String -> Parser ()
consumeStr [] xs = Right ((), xs)
consumeStr _ [] = Left []
consumeStr (s : ss) (x : xs)
  | x == s = consumeStr ss xs
  | otherwise = Left xs

consumeChar :: Char -> Parser ()
consumeChar c (x : xs)
  | x == c = Right ((), xs)
  | otherwise = Left xs
consumeChar _ s = Left $ safeTail s

consumeWhile :: (Char -> Bool) -> Parser String
consumeWhile p (x : xs)
  | p x = Right (x : digits, rest)
  | otherwise = Left xs
  where
    (digits, rest) = span p xs
consumeWhile _ [] = Left []

consume' :: Parser Int
consume' s = do
  (_, s) <- consumeStr "mul" s
  (_, s) <- consumeChar '(' s
  (d1, s) <- consumeWhile isDigit s
  (_, s) <- consumeChar ',' s
  (d2, s) <- consumeWhile isDigit s
  (_, s) <- consumeChar ')' s
  Right (read d1 * read d2, s)

consume :: String -> [Int]
consume "" = []
consume xs = case consume' xs of
  Left s -> consume s
  Right (x, s) -> x : consume s

part1 :: String -> Int
part1 = sum . consume

part2 :: String -> Int
part2 = const 2
