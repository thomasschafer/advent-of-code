module Main where

import Control.Arrow (Arrow (second))

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

data Node = Node
  { children :: [Node],
    metadata :: [Int]
  }
  deriving (Show)

parse :: String -> Node
parse = (\([n], []) -> n) . go 1 . map read . words
  where
    go 0 rest = ([], rest)
    go n (numChildren : numMetadata : rest) = (Node cs m : nodes, rest'')
      where
        (cs, (m, rest')) = second (splitAt numMetadata) $ go numChildren rest
        (nodes, rest'') = go (n - 1) rest'

sumMetadata :: Node -> Int
sumMetadata Node {children, metadata} = sum metadata + sum (map sumMetadata children)

part1 :: String -> Int
part1 = sumMetadata . parse

value :: Node -> Int
value (Node cs m)
  | null cs = sum m
  | otherwise = sum [value n | i <- m, i >= 1 && i <= length cs, let n = cs !! (i - 1)]

part2 :: String -> Int
part2 = value . parse
