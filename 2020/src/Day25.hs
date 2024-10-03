module Day25 (part1) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Utils ((...), toTuple)

transform :: Integer -> Integer -> Integer
transform = (`mod` 20201227) ... (*)

encryptionKey :: Integer -> Integer -> Integer
encryptionKey pubKey1 pubKey2 = privateKey
 where
  loopSize2 = fromJust . elemIndex pubKey2 $ iterate (transform 7) 1
  privateKey = (!! loopSize2) $ iterate (transform pubKey1) 1

part1 :: String -> Integer
part1 = uncurry encryptionKey . toTuple . map read . lines
