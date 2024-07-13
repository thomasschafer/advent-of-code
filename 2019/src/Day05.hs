module Day05 (part1, part1_day2_test, part1_day2_real, part2) where

import Data.List.Split (splitOn)
import Utils (lpad, quickTrace, toInt, updateAt)

data Mode = Position | Immediate
  deriving (Show) -- TODO: delete

toMode :: Int -> Mode
toMode 0 = Position
toMode 1 = Immediate

runProgram :: [Int] -> [Int] -> Int -> [Int] -> ([Int], [Int]) -- (opCodes, outputs)
runProgram inputs outputs curIdx opCodes
  -- TODO: do something with inputs and outputs
  | opCode == 99 = (opCodes, outputs)
  | opCode `elem` [1, 2] =
      let op = case opCode of
            1 -> (+)
            2 -> (*)
          paramsZipped = zip modes $ take 3 rest
          (_, writePos) = last paramsZipped
          [p1, p2] = map (uncurry toParam) $ init paramsZipped
       in runProgram inputs outputs (curIdx + 4) (updateAt writePos (p2 `op` p1) opCodes)
  | opCode == 3 =
      let writePos = head rest
       in runProgram (tail inputs) outputs (curIdx + 2) (updateAt writePos (head inputs) opCodes)
  | opCode == 4 =
      let output = head $ zipWith toParam head rest
       in runProgram inputs (output : outputs) (curIdx + 2) opCodes
  where
    (nextCode : rest) = drop curIdx opCodes
    opCode = nextCode `mod` 100
    modes = map (toMode . toInt) . reverse . lpad '0' 3 . show $ nextCode `div` 100

    toParam Immediate = id
    toParam Position = (opCodes !!)

solve :: [Int] -> [Int] -> Int
solve inputs = head . fst . runProgram inputs [] 0 . restoreProgram
  where
    restoreProgram = updateAt 2 2 . updateAt 1 12

parse :: (Read a) => String -> [a]
parse = map read . splitOn ","

part1_day2_test, part1_day2_real :: String -> Int -- TODO: delete
part1_day2_test = head . fst . runProgram [1] [] 0 . parse
part1_day2_real = solve [1] . parse

part1 :: String -> Int
part1 = head . quickTrace "res1" . snd . runProgram [1] [] 0 . parse

-- part2 :: String -> Int
-- part2 s =
--   head
--     [ 100 * noun + verb
--       | noun <- [0 .. 99],
--         verb <- [0 .. 99],
--         solve (noun, verb) opCodes == 19690720
--     ]
--   where
--     opCodes = parse s

part2 :: String -> Int
part2 = const 2
