module Day01 (part1, part2) where

solve :: Bool -> String -> Int
solve useIntermediate = snd . foldl apply (50, 0) . map parse . lines
  where
    parse (dir : rest) = sign dir * read rest
      where
        sign 'L' = -1
        sign 'R' = 1

    apply (pos, n) shift = (newPos, n + delta)
      where
        newPos = (pos + shift) `mod` 100

        cycles = abs (shift `quot` 100)
        inLastRot = pos' >= 100 || (pos /= 0 && pos' <= 0)
          where
            pos' = pos + (shift `rem` 100)
        delta =
          if useIntermediate
            then (cycles + fromEnum inLastRot)
            else (if newPos == 0 then 1 else 0)

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
