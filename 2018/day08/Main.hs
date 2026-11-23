module Main where

import Control.Arrow (Arrow (first))
import Data.Bifunctor (Bifunctor (..))
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)

-- When you start part 2, uncomment the next line.
-- print (part2 input)

data Node = Node
  { children :: [Node],
    metadata :: [Int]
  }
  deriving (Show)

parse :: String -> Node
parse = (\(n, []) -> n) . go . map read . words . head . lines
  where
    go :: [Int] -> (Node, [Int])
    go (numChildren : numMetadata : rest) = (Node {children = cs, metadata = m}, rest')
      where
        (cs, (m, rest')) = bimap reverse (splitAt numMetadata) $ iterate takeChild ([], rest) !! numChildren

        takeChild :: ([Node], [Int]) -> ([Node], [Int])
        takeChild (acc, rem) = (c : acc, rem')
          where
            (c, rem') = go rem

sumMetadata :: Node -> Int
sumMetadata Node {children, metadata} = sum metadata + sum (map sumMetadata children)

part1 :: String -> Int
part1 = sumMetadata . parse

part2 :: String -> Int
part2 = undefined
