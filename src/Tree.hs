module Tree where

import Data
import Data.List (group, maximumBy, sort)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Decision (decision)
import Text.Read (readMaybe)
import Debug.Trace (trace)

data DecisionTree = Node Constraints (DecisionTree, DecisionTree) | Leaf Label

instance Show DecisionTree where
  show = showTree 0
    where
      showTree :: Int -> DecisionTree -> String
      showTree depth (Leaf label) =
        indent depth ++ "Leaf: " ++ show label ++ "\n"
      showTree depth (Node constraints (left, right)) =
        indent depth
          ++ "Node: "
          ++ show constraints
          ++ "\n"
          ++ showTree (depth + 1) left
          ++ showTree (depth + 1) right

      indent :: Int -> String
      indent n = replicate (n * 2) ' '

---------------------------------------------------

classify :: DecisionTree -> Object -> Label
classify (Leaf c) _ = c
classify (Node constraints branches) obj =
  if satisfies constraints obj
    then classify (snd branches) obj
    else classify (fst branches) obj

------------------------------------------------

build :: [LabeledObject] -> DecisionTree
build table
  | null table = error "Cannot build tree from an empty table"
  | length table < 5 = Leaf (mostFrequentOrAverageLabel table)
  | allSameLabels table = Leaf (classOf (head table))
  | otherwise =
      let bestSplit = decision table
          (left, right) = splitTable table bestSplit
       in 
        if null left || null right
          then Leaf (classOf (head table))
          else Node [bestSplit] (build left, build right)

mostFrequentOrAverageLabel :: [LabeledObject] -> Label
mostFrequentOrAverageLabel table =
  let labels = map snd table
   in if allIsNumeric labels
        then show (averageDouble (map readNumeric labels))
        else mostFrequentLabel labels

allIsNumeric :: [Label] -> Bool
allIsNumeric = all isNumeric
  where
    isNumeric label = isJust (readMaybe label :: Maybe Int) || isJust (readMaybe label :: Maybe Double)

readNumeric :: Label -> Double
readNumeric label = case readMaybe label :: Maybe Double of
  Just n -> n
  Nothing -> error "Label is not a number"

averageDouble :: [Double] -> Double
averageDouble nums = sum nums / fromIntegral (length nums)

mostFrequentLabel :: [Label] -> Label
mostFrequentLabel labels =
  let grouped = group (sort labels)
      mostCommon = maximumBy (comparing length) grouped
   in head mostCommon

allSameLabels :: [LabeledObject] -> Bool
allSameLabels table =
  let labels = map snd table
   in all (== head labels) labels
