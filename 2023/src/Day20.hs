module Day20 (part1, part2) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Deque.Lazy (Deque)
import Deque.Lazy qualified as D
import Utils (quickTrace)

data Pulse = Low | High
  deriving (Eq, Show)

data ComModule
  = FlipFlop String Bool [String]
  | Conjunction String (HashMap String Pulse) [String]
  | Broadcast String [String]
  | Untyped String
  deriving (Show)

moduleName :: ComModule -> String
moduleName (FlipFlop name _ _) = name
moduleName (Conjunction name _ _) = name
moduleName (Broadcast name _) = name
moduleName (Untyped name) = name

moduleSendTo :: ComModule -> [String]
moduleSendTo (FlipFlop _ _ sendTos) = sendTos
moduleSendTo (Conjunction _ _ sendTos) = sendTos
moduleSendTo (Broadcast _ sendTos) = sendTos
moduleSendTo (Untyped _) = []

parse :: [String] -> HashMap String ComModule
parse = completeConjunctionInputMaps . foldl addModule HM.empty
  where
    addModule modules s = HM.insert (moduleName comModule) comModule modules
      where
        (nameWithPref, outputs) = (\[n, o] -> (n, splitOn ", " o)) $ splitOn " -> " s
        comModule
          | nameWithPref == "broadcaster" = Broadcast nameWithPref outputs
          | head nameWithPref == '%' = FlipFlop (tail nameWithPref) False outputs
          | head nameWithPref == '&' = Conjunction (tail nameWithPref) HM.empty outputs
          | otherwise = Untyped nameWithPref
    completeConjunctionInputMaps modules = foldl updateInputMaps modules (HM.elems modules)
    updateInputMaps modules m = foldl (updateInputMap (moduleName m)) modules (moduleSendTo m)
    updateInputMap from modules to = case HM.lookup to modules of
      Just (Conjunction name inputMap sendTos) ->
        HM.insert to (Conjunction name (HM.insert from Low inputMap) sendTos) modules
      _ -> modules

type PulseCounts = (Int, Int)

type ComModuleState = (Int, Deque (String, String, Pulse), HashMap String ComModule, PulseCounts)

updatePulseCounts :: PulseCounts -> Pulse -> Int -> PulseCounts
updatePulseCounts (low, high) Low num = (low + num, high)
updatePulseCounts (low, high) High num = (low, high + num)

initialState :: Int -> HashMap String ComModule -> PulseCounts -> ComModuleState
initialState = (,D.snoc ("button", "broadcaster", Low) mempty,,)

data TerminationCondition = Limit Int | PulseTo Pulse String

hasTerminatedLimit :: TerminationCondition -> Int -> Bool
hasTerminatedLimit (Limit limit) n = n > limit
hasTerminatedLimit (PulseTo _ _) _ = False

hasTerminatedPulseTo :: TerminationCondition -> Pulse -> String -> Bool
hasTerminatedPulseTo (PulseTo ptExp pulseExp) ptActual pulseActual =
  ptExp == ptActual && pulseExp == pulseActual
hasTerminatedPulseTo (Limit _) _ _ = False

propogateButtonPushes :: TerminationCondition -> ComModuleState -> Int
propogateButtonPushes termCond (n, pulses, modules, counts)
  | hasTerminatedLimit termCond n = uncurry (*) counts
  | D.null pulses = propogateButtonPushes termCond $ initialState (n + 1) modules counts
  | otherwise =
      if hasTerminatedPulseTo termCond pulseType pulseTo
        then n
        else propogateButtonPushes termCond (n, updatedPulses, updatedModules, updatedCounts)
  where
    ((pulseFrom, pulseTo, pulseType), rest) = fromJust $ D.uncons pulses
    updatedCounts = updatePulseCounts counts pulseType 1
    addPulse pt from = foldl (flip (D.snoc . (from,,pt))) rest

    (updatedPulses, updatedModules) = case HM.lookup pulseTo modules of
      Just (FlipFlop name isOn sendTos) ->
        case pulseType of
          High -> (rest, modules)
          Low ->
            ( addPulse (if not isOn then High else Low) name sendTos,
              HM.insert name (FlipFlop name (not isOn) sendTos) modules
            )
      Just (Conjunction name inputMap sendTos) ->
        ( addPulse (if all (== High) (HM.elems updatedInputMap) then Low else High) name sendTos,
          HM.insert name (Conjunction name updatedInputMap sendTos) modules
        )
        where
          updatedInputMap = HM.insert pulseFrom pulseType inputMap
      Just (Broadcast name sendTos) -> (addPulse pulseType name sendTos, modules)
      Just (Untyped _) -> (rest, modules)
      Nothing -> (rest, modules)

solve :: TerminationCondition -> String -> Int
solve termCond = propogateButtonPushes termCond . flip (initialState 1) (0, 0) . parse . lines

part1 :: String -> Int
part1 = solve (Limit 1000)

part2 :: String -> Int
-- part2 = solve (PulseTo Low "rx")
part2 = solve (PulseTo Low "inv")
