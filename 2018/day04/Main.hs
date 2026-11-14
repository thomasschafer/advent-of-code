module Main where

import Data.HashMap.Internal.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (maximumBy, sort, span)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.Time (LocalTime, addLocalTime, defaultTimeLocale, diffLocalTime, timeZoneMinutes)
import Data.Time.Format (parseTimeM)
import Data.Time.LocalTime (localTimeOfDay, timeOfDayToTime)
import Data.Tuple (swap)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- getContents
  print (part1 input)
  print (part2 input)

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

groupByGuard :: [(LocalTime, Event)] -> HashMap GuardId [(LocalTime, Event)]
groupByGuard = go HM.empty
  where
    go acc [] = acc
    go acc (e@(_, StartShift gid) : events) = go acc' rest
      where
        acc' = HM.insert gid ((fromMaybe [] $ HM.lookup gid acc) ++ e : gEvs) acc
        (gEvs, rest) = span (\(_, e) -> case e of StartShift _ -> False; _ -> True) events

calcSleepWindows :: HashMap GuardId [(LocalTime, Event)] -> HashMap GuardId [(LocalTime, LocalTime)]
calcSleepWindows = HM.map (groupSleeps [])
  where
    groupSleeps acc [] = acc
    groupSleeps acc ((_, StartShift _) : rest) = groupSleeps acc rest
    groupSleeps acc ((start, Sleep) : (end, Wake) : rest) = groupSleeps (acc ++ [(start, addLocalTime (-60) end)]) rest

mostCommonMinute :: [(LocalTime, LocalTime)] -> Int
mostCommonMinute windows = maxKeyByValue minuteMap
  where
    minuteMap :: HashMap Int Int
    minuteMap = foldl update HM.empty $ concatMap toMinutes windows
      where
        update :: HashMap Int Int -> Int -> HashMap Int Int
        update acc minute = HM.insert minute (1 + fromMaybe 0 (HM.lookup minute acc)) acc

toMinutes :: (LocalTime, LocalTime) -> [Int]
toMinutes (start, end) = [(toMins start) .. (toMins end)]
  where
    toMins = (`div` 60) . floor . timeOfDayToTime . localTimeOfDay

maxKeyByValue :: (Eq a, Hashable a, Ord b) => HashMap a b -> a
maxKeyByValue = fst . maximumBy (comparing snd) . HM.toList

part1 :: String -> Int
part1 = solve . calcSleepWindows . groupByGuard . map parse . sort . lines
  where
    solve :: HashMap GuardId [(LocalTime, LocalTime)] -> Int
    solve sleepWindows = (* sleepiestGuard) . mostCommonMinute . fromJust $ HM.lookup sleepiestGuard sleepWindows
      where
        sleepList = traceShowId $ HM.map (sum . map (uncurry $ flip diffLocalTime)) sleepWindows
        sleepiestGuard = maxKeyByValue sleepList

toGuardSleepCounts :: HashMap GuardId [(LocalTime, LocalTime)] -> HashMap (GuardId, Int) Int
toGuardSleepCounts = foldl update HM.empty . HM.toList
  where
    update :: HashMap (GuardId, Int) Int -> (GuardId, [(LocalTime, LocalTime)]) -> HashMap (GuardId, Int) Int
    update acc (guard, mins) = foldl incr acc . map (guard,) $ concatMap toMinutes mins
      where
        foo = map toMinutes mins

incr :: (Hashable a) => HashMap a Int -> a -> HashMap a Int
incr h k = HM.insert k (1 + fromMaybe 0 (HM.lookup k h)) h

part2 :: String -> Int
part2 = uncurry (*) . maxKeyByValue . toGuardSleepCounts . calcSleepWindows . groupByGuard . map parse . sort . lines
