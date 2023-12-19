module Day19 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)

data Category = X | M | A | S

data Rule = LessThan Category Int String | GreaterThan Category Int String | GoTo String

data MachinePart = MachinePart {x :: Int, m :: Int, a :: Int, s :: Int}

readCat :: Char -> Category
readCat c = case c of
  'x' -> X
  'm' -> M
  'a' -> A
  's' -> S
  _ -> error [c]

parseRule :: String -> Rule
parseRule s
  | '<' `elem` s =
      let (category, num, goto) = parse "<"
       in LessThan category num goto
  | '>' `elem` s =
      let (category, num, goto) = parse ">"
       in GreaterThan category num goto
  | otherwise = GoTo s
  where
    parse c = (\[[[cat], num], [goto]] -> (readCat cat, read num, goto)) . map (splitOn c) $ splitOn ":" s

parseRules :: String -> (String, [Rule])
parseRules s = (workflowName, map parseRule rules)
  where
    [workflowName, rulesStr] = splitOn "{" s
    rules = splitOn "," $ init rulesStr

parsePart :: String -> MachinePart
parsePart s = foldl update (MachinePart {}) partPairs
  where
    partPairs = map ((\[[a], b] -> (readCat a, read b)) . splitOn "=") . splitOn "," . init $ tail s
    update p (category, num) = case category of
      X -> p {x = num}
      M -> p {m = num}
      A -> p {a = num}
      S -> p {s = num}

parse :: String -> (HashMap String [Rule], [MachinePart])
parse s = (HM.fromList (map parseRules rules), map parsePart parts)
  where
    [rules, parts] = map lines $ splitOn "\n\n" s

matchesWithPred :: (Int -> Int -> Bool) -> MachinePart -> (Category, Int, String) -> Maybe String
matchesWithPred predicate (MachinePart {x, m, a, s}) (cat, num, newWf) = if matches then Just newWf else Nothing
  where
    matches = case cat of
      X -> x `predicate` num
      M -> m `predicate` num
      A -> a `predicate` num
      S -> s `predicate` num

matchesRule :: MachinePart -> Rule -> Maybe String
matchesRule part (LessThan cat num newWf) = matchesWithPred (<) part (cat, num, newWf)
matchesRule part (GreaterThan cat num newWf) = matchesWithPred (>) part (cat, num, newWf)
matchesRule _ (GoTo newWf) = Just newWf

acceptedParts :: (HashMap String [Rule], [MachinePart]) -> [MachinePart]
acceptedParts (ruleMap, parts) = filter (isAccepted "in") parts
  where
    isAccepted workflow part = case newWorkflow of
      "A" -> True
      "R" -> False
      wf -> isAccepted wf part
      where
        newWorkflow = head . mapMaybe (matchesRule part) . fromJust $ HM.lookup workflow ruleMap

part1 :: String -> Int
part1 = sum . map (\MachinePart {x, m, a, s} -> x + m + a + s) . acceptedParts . parse

data NumRanges = NumRanges
  { xRange :: (Int, Int),
    mRange :: (Int, Int),
    aRange :: (Int, Int),
    sRange :: (Int, Int)
  }

updateRange :: NumRanges -> Rule -> (NumRanges, NumRanges)
updateRange range@(NumRanges {xRange, mRange, aRange, sRange}) (LessThan cat num _) =
  case cat of
    X -> (range {xRange = updatedRange xRange}, range {xRange = remainingRange xRange})
    M -> (range {mRange = updatedRange mRange}, range {mRange = remainingRange mRange})
    A -> (range {aRange = updatedRange aRange}, range {aRange = remainingRange aRange})
    S -> (range {sRange = updatedRange sRange}, range {sRange = remainingRange sRange})
  where
    updatedRange (start, end) = (start, min end (num - 1))
    remainingRange (start, end) = (max start num, end)
updateRange range@(NumRanges {xRange, mRange, aRange, sRange}) (GreaterThan cat num _) =
  case cat of
    X -> (range {xRange = updatedRange xRange}, range {xRange = remainingRange xRange})
    M -> (range {mRange = updatedRange mRange}, range {mRange = remainingRange mRange})
    A -> (range {aRange = updatedRange aRange}, range {aRange = remainingRange aRange})
    S -> (range {sRange = updatedRange sRange}, range {sRange = remainingRange sRange})
  where
    updatedRange (start, end) = (max start (num + 1), end)
    remainingRange (start, end) = (start, min end num)

isValidRange :: NumRanges -> Bool
isValidRange (NumRanges {xRange, mRange, aRange, sRange}) =
  all (\(start, end) -> end >= start) [xRange, mRange, aRange, sRange]

validRanges :: HashMap String [Rule] -> [NumRanges]
validRanges ruleMap =
  followRanges (fromJust $ HM.lookup "in" ruleMap) $
    NumRanges {xRange = (1, 4000), mRange = (1, 4000), aRange = (1, 4000), sRange = (1, 4000)}
  where
    goto (LessThan _ _ to) = to
    goto (GreaterThan _ _ to) = to

    followRanges [] _ = []
    followRanges ((GoTo "A") : _) range = [range]
    followRanges ((GoTo "R") : _) _ = []
    followRanges ((GoTo to) : _) range = followRanges (fromJust $ HM.lookup to ruleMap) range
    followRanges (rule : rules) range =
      concat $
        [ case goto rule of
            "A" -> [updatedRange]
            "R" -> []
            to -> followRanges (fromJust $ HM.lookup to ruleMap) updatedRange
          | isValidRange updatedRange
        ]
          ++ [followRanges rules remainingRange | isValidRange remainingRange]
      where
        (updatedRange, remainingRange) = updateRange range rule

numCombinations :: NumRanges -> Int
numCombinations NumRanges {xRange, mRange, aRange, sRange} =
  product $ map ((+ 1) . uncurry (flip (-))) [xRange, mRange, aRange, sRange]

part2 :: String -> Int
part2 = sum . map numCombinations . validRanges . fst . parse
