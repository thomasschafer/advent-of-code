module Main where

import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sort, span)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Time (LocalTime, addLocalTime, defaultTimeLocale, diffLocalTime, timeZoneMinutes)
import Data.Time.Format (parseTimeM)
import Data.Tuple (swap)
import Data.Time.LocalTime (localTimeOfDay)
import Data.Time.LocalTime (timeOfDayToTime)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)

-- When you start part 2, uncomment the next line.
-- print (part2 input)

type GuardId = Int

data Event = StartShift GuardId | Sleep | Wake

parseDateTime :: String -> LocalTime
parseDateTime = fromJust . parseTimeM True defaultTimeLocale "[%Y-%m-%d %H:%M]"

parse :: String -> (LocalTime, Event)
parse s = (parseDateTime (dateStr ++ " " ++ timeStr), event)
  where
    ([dateStr, timeStr], eventStrList) = splitAt 2 $ words s

    event = case eventStrList of
      ["Guard", ('#' : guardIdStr), "begins", "shift"] -> StartShift (read guardIdStr)
      ["falls", "asleep"] -> Sleep
      ["wakes", "up"] -> Wake

part1 :: String -> Int
part1 = solve . groupByGuard HM.empty . map parse . sort . lines

groupByGuard :: HashMap GuardId [(LocalTime, Event)] -> [(LocalTime, Event)] -> HashMap GuardId [(LocalTime, Event)]
groupByGuard acc [] = acc
groupByGuard acc (e@(_, StartShift gid) : events) = groupByGuard acc' rest
  where
    acc' = HM.insert gid ((fromMaybe [] $ HM.lookup gid acc) ++ e : gEvs) acc
    (gEvs, rest) = span (\(_, e) -> case e of StartShift _ -> False; _ -> True) events

solve :: HashMap GuardId [(LocalTime, Event)] -> Int
solve h = (*sleepiestGuard) . mostCommonMinute . fromJust $ HM.lookup sleepiestGuard sleepWindows
  where
    sleepWindows = traceShowId $ HM.map (groupSleeps []) h
      where
        groupSleeps acc [] = acc
        groupSleeps acc ((_, StartShift _) : rest) = groupSleeps acc rest
        groupSleeps acc ((start, Sleep) : (end, Wake) : rest) = groupSleeps (acc ++ [(start, addLocalTime (-60) end)]) rest

    sleepList = traceShowId $ HM.map (sum . map (uncurry $ flip  diffLocalTime)) sleepWindows
    sleepiestGuard = traceShowId . snd . maximum . map swap $ HM.toList sleepList

mostCommonMinute :: [(LocalTime, LocalTime)] -> Int
mostCommonMinute windows = snd . maximum . map swap $ HM.toList minuteMap
  where
    minuteMap :: HashMap Int Int
    minuteMap = foldl update HM.empty $ concatMap toMinutes windows
      where
        update :: HashMap Int Int -> Int -> HashMap Int Int
        update acc minute = HM.insert minute (1 + fromMaybe 0 (HM.lookup minute acc)) acc

toMinutes :: (LocalTime, LocalTime) -> [Int]
toMinutes (start, end) = [(toMins start)..(toMins end)]
  where
    toMins = (`div` 60) . floor . timeOfDayToTime . localTimeOfDay

part2 :: String -> Int
part2 = undefined
