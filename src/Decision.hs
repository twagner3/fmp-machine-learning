module Decision where

import Data
import Data.List (minimumBy)
import Data.Ord (comparing)

decision :: [LabeledObject] -> Restriction
decision table =
  let objects = map fst table
      attributes = distinctAttr objects
      restrictions = map (attributeRestriction table) attributes
      bestRestriction = minRestriction restrictions
   in fst bestRestriction

distinctAttr :: [Object] -> [Attr]
distinctAttr objects = uniqueHelper [attr | object <- objects, attr <- getAttr object] []

attributeRestriction :: [LabeledObject] -> Attr -> (Restriction, Double)
attributeRestriction table attribute =
  let restrictions = map createRestrictions (distinctValues (map fst table) attribute)
      gini_values = map (calculateGini table) restrictions
   in minRestriction (zip restrictions gini_values)

distinctValues :: [Object] -> Attr -> [Feature]
distinctValues table attribute = uniqueHelper (concatMap (filter (\f -> extractAttr f == attribute)) table) []

createRestrictions :: Feature -> Restriction
createRestrictions (ADouble name value) = Order name Data.LTE value
createRestrictions (AStr name value) = Equal name value

calculateGini :: [LabeledObject] -> Restriction -> Double
calculateGini table restriction =
  let (left, right) = splitTable table restriction

      gini :: [LabeledObject] -> Double
      gini group =
        let total = fromIntegral (length group)
            labels = map snd group
            uniqueLabels = uniqueHelper labels []
            labelCounts = map (\lbl -> length (filter (== lbl) labels)) uniqueLabels
            probabilities = map (\count -> fromIntegral count / total) labelCounts
         in 1 - sum (map (\p -> p * p) probabilities)

      leftWeight = fromIntegral (length left) / fromIntegral (length table)
      rightWeight = fromIntegral (length right) / fromIntegral (length table)
   in leftWeight * gini left + rightWeight * gini right

minRestriction :: [(Restriction, Double)] -> (Restriction, Double)
minRestriction = minimumBy (comparing snd)

uniqueHelper :: (Eq a) => [a] -> [a] -> [a]
uniqueHelper [] seen = seen
uniqueHelper (x : xs) seen
  | x `elem` seen = uniqueHelper xs seen
  | otherwise = uniqueHelper xs (seen ++ [x])
