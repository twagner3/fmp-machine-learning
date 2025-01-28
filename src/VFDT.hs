module VFDT where

import Data
import Data.List (maximumBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Text.Read (readMaybe)

-- XNode type represents a list of attributes
type XNode = [Attr]

type Statistics = Map.Map Label Int
type NTable = Map.Map Feature Statistics

data DecisionTree = Leaf Int XNode Label NTable | Node Int XNode Restriction (DecisionTree, DecisionTree) NTable

instance Show DecisionTree where
  show = showTree 0
    where
      showTree :: Int -> DecisionTree -> String
      showTree depth (Leaf _ _ label _) =
        indent depth ++ "Leaf: " ++ show label ++ "\n"
      showTree depth (Node _ _ restriction (left, right) _) =
        indent depth
          ++ "Node: "
          ++ show restriction
          ++ "\n"
          ++ showTree (depth + 1) left
          ++ showTree (depth + 1) right

      indent :: Int -> String
      indent n = replicate (n * 2) ' '

----------------------------------------

emptyTable :: NTable
emptyTable = Map.empty

-- Computes the Hoeffding bound for estimating the required number of data points
hoeffdingBound :: Int -> Int -> Double -> Double
hoeffdingBound r n delta = sqrt ((fromIntegral r * fromIntegral r * log (1 / delta)) / (2 * fromIntegral n))

-- Splits an NTable into two parts based on a given restriction
splitNTable :: NTable -> Restriction -> (NTable, NTable)
splitNTable nTable restriction =
  let (restricted, others) = Map.partitionWithKey (\feature _ -> matchesAttribute restriction feature) nTable
      satisfiesR = Map.filterWithKey (\feature _ -> satisfiesConstraint restriction [feature]) restricted
      notSatisfies = Map.filterWithKey (\feature _ -> not (satisfiesConstraint restriction [feature])) restricted
   in (Map.union satisfiesR others, Map.union notSatisfies others)

-- Updates the NTable based on a new LabeledObject
incrementInNTable :: LabeledObject -> NTable -> NTable
incrementInNTable (object, objLabel) nTable = foldl (updateFeature objLabel) nTable object
  where
    updateFeature :: Label -> NTable -> Feature -> NTable
    updateFeature label table feature = Map.insertWith (Map.unionWith (+)) feature (Map.singleton label 1) table

-- Builds a new decision tree from a list of LabeledObjects
buildNew :: [LabeledObject] -> DecisionTree
buildNew objects =
  let leaf = Leaf 0 (extractUniqueAttrs objects ++ [getAttrOfRestriction nullRestriction]) "null" emptyTable
   in foldl learn leaf objects

-- Learns from a new LabeledObject and updates the decision tree
learn :: DecisionTree -> LabeledObject -> DecisionTree
learn (Node n x restriction children table) object =
  let uTable = incrementInNTable object table
      node = reEvalSplit (Node (n + 1) x restriction children uTable)
   in learnNext node object
learn (Leaf n x label table) object =
  let uTable = incrementInNTable object table
      leaf = attemptSplit (Leaf (n + 1) x label uTable)
   in learnNext leaf object

-- Decides the next step after learning from an object, whether to continue with the current node or to split further
learnNext :: DecisionTree -> LabeledObject -> DecisionTree
learnNext (Node n x restriction (left, right) table) object =
  if satisfiesConstraint restriction (fst object)
    then Node n x restriction (learn left object, right) table
    else Node n x restriction (left, learn right object) table
learnNext (Leaf n x label table) _ = Leaf n x label table

-- Re-evaluates if a split should be done at a node based on information gain and Hoeffding bound
reEvalSplit :: DecisionTree -> DecisionTree
reEvalSplit (Node n x restriction (left, right) table) =
  let gAttributes = informationGainForAttrs x table
      xA = maximumBy (comparing snd) gAttributes
      xC = restriction
      epsilon = hoeffdingBound (distinctLabelsCount table) n 0.05
   in if enoughDatePoints n && snd xA - informationGainForRestriction table xC > epsilon
        then
          if fst xA == nullRestriction
            then trace ("Kill subtree") $ Leaf n x "null" table
            else
              if fst xA /= restriction
                then -- trace ("ReEvalSplit changed split") $
                  replaceWithInternal (Leaf n x "split" table) (fst xA)
                else Node n x restriction (left, right) table
        else Node n x restriction (left, right) table
reEvalSplit leaf = leaf

-- Attempts to split a leaf node if certain conditions are met
attemptSplit :: DecisionTree -> DecisionTree
attemptSplit (Leaf n x l table) =
  if enoughDatePoints n && not (allEntriesHaveSameLabel table)
    then
      let gAttributes = informationGainForAttrs x table
          xA = maximumBy (comparing snd) gAttributes
          epsilon = hoeffdingBound (distinctLabelsCount table) n 0.05
       in -- trace("Result " ++ show epsilon ++ show (xA)) $
          if snd xA - informationGainForRestriction table nullRestriction > epsilon && fst xA /= nullRestriction
            then -- trace ("Attempt split added split") $
              replaceWithInternal (Leaf n x l table) (fst xA)
            else Leaf n x (majorityLabel table) table
    else Leaf n x (majorityLabel table) table
attemptSplit node = node

-- Replaces a leaf with an internal node based on a given restriction
replaceWithInternal :: DecisionTree -> Restriction -> DecisionTree
replaceWithInternal (Leaf n x l table) restriction =
  let leftAfter = Leaf 0 x l emptyTable
      rightAfter = Leaf 0 x l emptyTable
   in Node n x restriction (leftAfter, rightAfter) table
replaceWithInternal (Node n x _ _ table) restriction =
  let leftAfter = Leaf 0 x "null" emptyTable
      rightAfter = Leaf 0 x "null" emptyTable
   in Node n x restriction (leftAfter, rightAfter) table

-- A null restriction used as a default or "no restriction"
nullRestriction :: Restriction
nullRestriction = Order "null" Data.LT 0

-- Function to determine if a label is numeric or empty
isNumericOrEmpty :: Label -> Bool
isNumericOrEmpty "" = True
isNumericOrEmpty label = isJust (readMaybe label :: Maybe Int) || isJust (readMaybe label :: Maybe Double)

-- Function to parse a numeric label as Double, treating empty strings as 0
readNumeric :: Label -> Double
readNumeric "" = 0
readNumeric label = case readMaybe label :: Maybe Double of
  Just n -> n
  Nothing -> error "Label is not a number"

-- Function to calculate the average of a list of Doubles
averageDouble :: [Double] -> Double
averageDouble nums = sum nums / fromIntegral (length nums)

-- Updated majorityLabel function to return the most frequent label or average if numeric
majorityLabel :: NTable -> Label
majorityLabel table =
  let combinedSums = Map.unionsWith (+) $ Map.elems table
      labels = Map.keys combinedSums
   in if all isNumericOrEmpty labels
        then show (averageDouble (map readNumeric labels))
        else fst $ maximumBy (comparing snd) $ Map.toList combinedSums

-- Classifies an object based on a decision tree
classify :: DecisionTree -> Object -> Label
classify (Leaf _ _ c _) _ = c
classify (Node _ _ constraints branches _) obj =
  if satisfiesConstraint constraints obj
    then classify (fst branches) obj
    else classify (snd branches) obj

-- Checks if a feature matches a given restriction
matchesAttribute :: Restriction -> Feature -> Bool
matchesAttribute (Equal attr _) (AStr featureAttr _) =
  attr == featureAttr
matchesAttribute (Equal attr _) (ADouble featureAttr _) =
  attr == featureAttr
matchesAttribute (Order attr _ _) (ADouble featureAttr _) =
  attr == featureAttr
matchesAttribute (Order attr _ _) (AStr featureAttr _) =
  attr == featureAttr

-- Checks if all entries in an NTable have the same label
allEntriesHaveSameLabel :: NTable -> Bool
allEntriesHaveSameLabel ntable =
  let relevantLabels = concatMap extractRelevantLabels (Map.elems ntable)
   in case relevantLabels of
        [] -> True
        (x : xs) -> all (== x) xs

-- Extracts the relevant labels (those with a count > 0) from statistics
extractRelevantLabels :: Statistics -> [Label]
extractRelevantLabels stats =
  Map.keys $ Map.filter (> 0) stats

-- Calculates the information gain for a given restriction
informationGainForRestriction :: NTable -> Restriction -> Double
informationGainForRestriction nTable restriction =
  let originalEntropy = totalEntropy nTable
      (leftTable, rightTable) = splitNTable nTable restriction
      leftCounts = sum [sum (Map.elems stats) | stats <- Map.elems leftTable]
      rightCounts = sum [sum (Map.elems stats) | stats <- Map.elems rightTable]
      totalCounts = leftCounts + rightCounts
      leftWeight = fromIntegral leftCounts / fromIntegral totalCounts
      rightWeight = fromIntegral rightCounts / fromIntegral totalCounts
      leftEntropy = totalEntropy leftTable
      rightEntropy = totalEntropy rightTable
   in if totalCounts > 0
        then originalEntropy - (leftWeight * leftEntropy + rightWeight * rightEntropy)
        else 0.0

-- Calculates the information gain for a single attribute
informationGainForAttr :: Attr -> NTable -> Maybe (Restriction, Double)
informationGainForAttr attr nTable =
  let restrictions = possibleRestrictionsWithAttr nTable attr
      gains = [(r, informationGainForRestriction nTable r) | r <- restrictions]
   in if not (null gains) then Just (maximumBy (\(_, g1) (_, g2) -> compare g1 g2) gains) else Nothing

-- Calculates the information gain for multiple attributes
informationGainForAttrs :: [Attr] -> NTable -> [(Restriction, Double)]
informationGainForAttrs attrs nTable =
  catMaybes [informationGainForAttr attr nTable | attr <- attrs]

-- Computes the total entropy of an NTable
totalEntropy :: NTable -> Double
totalEntropy nTable =
  let labelCounts = concatMap Map.elems (Map.elems nTable)
      totalCount = fromIntegral (sum labelCounts)
      probs = map (\count -> let p = fromIntegral count / totalCount in p * logBase 2 p) labelCounts
   in -sum probs

-- Generates possible restrictions for a given attribute in an NTable
possibleRestrictionsWithAttr :: NTable -> Attr -> [Restriction]
possibleRestrictionsWithAttr table requiredAttr =
  [Order attr Data.LTE value | (ADouble attr value) <- Map.keys table, attr == requiredAttr]
    ++ [Equal attr value | (AStr attr value) <- Map.keys table, attr == requiredAttr]

-- Counts the number of distinct labels in an NTable
distinctLabelsCount :: NTable -> Int
distinctLabelsCount nTable =
  let allLabels = concatMap Map.keys (Map.elems nTable)
      uniqueLabels = removeDuplicates allLabels
   in length uniqueLabels

-- Removes duplicate elements from a list
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

-- Checks if there are enough data points based on the number of entries
enoughDatePoints :: Int -> Bool
enoughDatePoints n = n `mod` 10 == 0
