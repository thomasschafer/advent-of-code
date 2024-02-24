module Day04 (part1, part2) where

import Data.List.Split (splitOn)

type Passport = [(String, String)]

solve :: (Passport -> Bool) -> String -> Int
solve p = length . filter p . map parsePassport . splitOn "\n\n"
  where
    parsePassport = map ((\[k, v] -> (k, v)) . splitOn ":") . words

isValidPassport1 :: Passport -> Bool
isValidPassport1 passport = all (`elem` map fst passport) expected
  where
    expected = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 :: String -> Int
part1 = solve isValidPassport1

between :: String -> Int -> Int -> Bool
between numStr lower upper = num >= lower && num <= upper
  where
    num = read numStr

lookupBetween :: Passport -> String -> Int -> Int -> Bool
lookupBetween passport key lower upper = case lookup key passport of
  Just s -> between s lower upper
  Nothing -> False

isValidPassport2 :: Passport -> Bool
isValidPassport2 passport =
  and
    [ lookupBetween passport "byr" 1920 2002,
      lookupBetween passport "iyr" 2010 2020,
      lookupBetween passport "eyr" 2020 2030,
      case lookup "hgt" passport of
        Just hgt -> case splitAt (length hgt - 2) hgt of
          (numStr, "cm") -> between numStr 150 193
          (numStr, "in") -> between numStr 59 76
          _ -> False
        Nothing -> False,
      case lookup "hcl" passport of
        Just ('#' : rest) -> length rest == 6 && all (`elem` (['0' .. '9'] ++ ['a' .. 'f'])) rest
        _ -> False,
      case lookup "ecl" passport of
        Just ecl -> ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        Nothing -> False,
      case lookup "pid" passport of
        Just nums -> length nums == 9 && all (`elem` ['0' .. '9']) nums
        _ -> False
    ]

part2 :: String -> Int
part2 = solve isValidPassport2