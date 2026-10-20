module Main (main) where


main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

fuelRequired :: Bool -> Int -> Int
fuelRequired recursiveFuel mass
  | fuel > 0 = sum (fuel : [fuelRequired recursiveFuel fuel | recursiveFuel])
  | otherwise = 0
 where
  fuel = mass `div` 3 - 2

solve :: Bool -> String -> Int
solve recursiveFuel = sum . map (fuelRequired recursiveFuel . read) . lines

part1 :: String -> Int
part1 = solve False

part2 :: String -> Int
part2 = solve True
