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
consumeChar = consumeStr . (: [])

consumeWhile :: (Char -> Bool) -> Parser String
consumeWhile p (x : xs)
  | p x = Right (x : digits, rest)
  | otherwise = Left xs
  where
    (digits, rest) = span p xs
consumeWhile _ [] = Left []

consumeMul :: Parser Int
consumeMul s = do
  (_, s) <- consumeStr "mul" s
  (_, s) <- consumeChar '(' s
  (d1, s) <- consumeWhile isDigit s
  (_, s) <- consumeChar ',' s
  (d2, s) <- consumeWhile isDigit s
  (_, s) <- consumeChar ')' s
  return (read d1 * read d2, s)

consumeAll :: Bool -> Bool -> Parser Int
consumeAll _ _ "" = Left ""
consumeAll respectEnable _ ('d' : 'o' : '(' : ')' : rest) = consumeAll respectEnable True rest
consumeAll respectEnable _ ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : rest) = consumeAll respectEnable False rest
consumeAll respectEnable enabled s
  | enabled || not respectEnable = case consumeMul s of
      Left s' -> consumeAll respectEnable enabled s'
      Right (x, s') -> Right (x, s')
  | otherwise = consumeAll respectEnable enabled $ safeTail s

solve :: Bool -> String -> Int
solve _ "" = 0
solve respectEnable xs = case consumeAll respectEnable True xs of
  Left s -> solve respectEnable s
  Right (x, s) -> x + solve respectEnable s

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
