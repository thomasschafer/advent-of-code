module Day21 (part1, part2) where

import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (toTuple)

type Allergen = String
type Ingredient = String

data Food = Food
  { ingredients :: Set Ingredient
  , allergens :: Set Allergen
  }
  deriving (Show)

parse :: String -> [Food]
parse =
  map (uncurry Food . toTuple . map (S.fromList . words . filter (/= ',') . init) . splitOn "(contains ")
    . lines

possibleIngredientsForAllergens :: [Food] -> HashMap Allergen (Set Ingredient)
possibleIngredientsForAllergens foods =
  HM.fromList
    [ ( allergen
      , foldl1 S.intersection . map ingredients $ filter ((allergen `elem`) . allergens) foods
      )
    | allergen <- S.toList . foldl1 S.union $ map allergens foods
    ]

part1 :: String -> Int
part1 s = length $ concatMap (filter (`elem` impossibleAllergens) . S.toList . ingredients) foods
 where
  foods = parse s
  allIngredients = foldl1 S.union $ map ingredients foods
  impossibleAllergens =
    allIngredients
      `S.difference` foldl1 S.union (HM.elems $ possibleIngredientsForAllergens foods)

findIngredients :: HashMap Allergen (Set Ingredient) -> HashMap Allergen Ingredient
findIngredients m
  | all ((== 1) . length) $ HM.elems m = HM.map (head . S.toList) m
  | any null $ HM.elems m = error $ "Found null element in mapping: " ++ show m
  | otherwise =
      findIngredients $
        HM.map (\ings -> if length ings == 1 then ings else S.filter (`notElem` singletons) ings) m
 where
  singletons = foldl (\acc alls -> if length alls == 1 then alls `S.union` acc else acc) S.empty $ HM.elems m

part2 :: String -> String
part2 =
  intercalate ","
    . map snd
    . sortBy (compare `on` fst)
    . HM.toList
    . findIngredients
    . possibleIngredientsForAllergens
    . parse
