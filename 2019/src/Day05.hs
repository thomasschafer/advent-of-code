module Day05 (part1, part2) where

import Data.List.Split (splitOn)
import Utils (lpad, toInt, updateAt)

data Mode = Position | Immediate

toMode :: Int -> Mode
toMode 0 = Position
toMode 1 = Immediate

runProgram :: [Int] -> [Int] -> Int -> [Int] -> ([Int], [Int]) -- (opCodes, outputs)
runProgram inputs outputs curIdx opCodes
  | opCode == 99 = (opCodes, outputs)
  | opCode `elem` [1, 2] =
      let op = case opCode of
            1 -> (+)
            2 -> (*)
          paramsZipped = zip modes $ take 3 rest
          (_, writeIdx) = last paramsZipped
          [p1, p2] = map (uncurry toParam) $ init paramsZipped
       in runProgram inputs outputs (curIdx + 4) (updateAt writeIdx (p2 `op` p1) opCodes)
  | opCode == 3 =
      let writeIdx = head rest
       in runProgram (tail inputs) outputs (curIdx + 2) (updateAt writeIdx (head inputs) opCodes)
  | opCode == 4 =
      let output = head $ zipWith toParam modes rest
       in runProgram inputs (output : outputs) (curIdx + 2) opCodes
  | opCode `elem` [5, 6] =
      let (param1 : param2 : _) = zipWith toParam modes rest
          pred = case opCode of
            5 -> (/=)
            6 -> (==)
       in runProgram inputs outputs (if param1 `pred` 0 then param2 else curIdx + 3) opCodes
  | opCode `elem` [7, 8] =
      let writeIdx = rest !! 2
          [param1, param2] = zipWith toParam modes $ take 2 rest
          pred = case opCode of
            7 -> (<)
            8 -> (==)
       in runProgram inputs outputs (curIdx + 4) (updateAt writeIdx (if param1 `pred` param2 then 1 else 0) opCodes)
  where
    (nextCode : rest) = drop curIdx opCodes
    opCode = nextCode `mod` 100
    modes = map (toMode . toInt) . reverse . lpad '0' 3 . show $ nextCode `div` 100

    toParam Immediate = id
    toParam Position = (opCodes !!)

solve :: [Int] -> String -> Int
solve inputs = head . snd . runProgram inputs [] 0 . map read . splitOn ","

part1 :: String -> Int
part1 = solve [1]

part2 :: String -> Int
part2 = solve [5]
