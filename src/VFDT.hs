module VFDT where

import Data
import Data.List (maximumBy)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Debug.Trace (trace)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

type XNode = [Attr]

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

type Statistics = Map.Map Label Int
type NTable = Map.Map Feature Statistics

----------------------------------------

emptyTable :: NTable
emptyTable = Map.empty

hoeffdingBound :: Int -> Int -> Double -> Double
hoeffdingBound r n delta = sqrt ((fromIntegral r * fromIntegral r * log (1 / delta)) / (2 * fromIntegral n))

splitNTable :: NTable -> Restriction -> (NTable, NTable)
splitNTable nTable restriction =
  let (restricted, others) = Map.partitionWithKey (\feature _ -> matchesAttribute restriction feature) nTable
      satisfies = Map.filterWithKey (\feature _ -> satisfiesConstraint restriction [feature]) restricted
      notSatisfies = Map.filterWithKey (\feature _ -> not (satisfiesConstraint restriction [feature])) restricted
   in (Map.union satisfies others, Map.union notSatisfies others)

incrementInNTable :: LabeledObject -> NTable -> NTable
incrementInNTable (object, objLabel) nTable = foldl (updateFeature objLabel) nTable object
  where
    updateFeature :: Label -> NTable -> Feature -> NTable
    updateFeature label table feature = Map.insertWith (Map.unionWith (+)) feature (Map.singleton label 1) table

buildNew :: [LabeledObject] -> DecisionTree
buildNew objects =
  let leaf = Leaf 0 (extractUniqueAttrs objects ++ [getAttrOfRestriction nullRestriction]) "null" emptyTable
   in foldl learn leaf objects

learn :: DecisionTree -> LabeledObject -> DecisionTree
learn (Node n x restriction children table) object =
  let uTable = incrementInNTable object table
      node = reEvalSplit (Node (n + 1) x restriction children uTable)
   in learnNext node object
learn (Leaf n x label table) object =
  let uTable = incrementInNTable object table
      leaf = attemptSplit (Leaf (n + 1) x label uTable)
   in learnNext leaf object

learnNext :: DecisionTree -> LabeledObject -> DecisionTree
learnNext (Node n x restriction (left, right) table) object =
  if satisfiesConstraint restriction (fst object)
    then Node n x restriction (learn left object, right) table
    else Node n x restriction (left, learn right object) table
learnNext (Leaf n x label table) _ = Leaf n x label table

reEvalSplit :: DecisionTree -> DecisionTree
reEvalSplit (Node n x restriction (left, right) table) =
  let gAttributes = informationGainForAttrs x table
      xA = maximumBy (comparing snd) gAttributes
      xC = restriction
      epsilon = hoeffdingBound (distinctLabelsCount table) n 0.05
   in if enoughDatePoints n && snd xA - informationGainForRestriction table xC > epsilon
        then
          if fst xA == getAttrOfRestriction nullRestriction
            then trace ("Kill subtree") $ Leaf n x "null" table
            else
              if fst xA /= getAttrOfRestriction restriction
                then trace ("ReEvalSplit changed split") $ replaceWithInternal (Leaf n x "split" table) (fst xA)
                else Node n x restriction (left, right) table
        else Node n x restriction (left, right) table
reEvalSplit leaf = leaf

attemptSplit :: DecisionTree -> DecisionTree
attemptSplit (Leaf n x l table) =
  if enoughDatePoints n && not (allEntriesHaveSameLabel table)
    then 
      let gAttributes = informationGainForAttrs x table
          xA = maximumBy (comparing snd) gAttributes
          epsilon = hoeffdingBound (distinctLabelsCount table) n 0.05
       in -- trace("Result " ++ show epsilon ++ show (xA)) $
        if snd xA - informationGainForRestriction table nullRestriction > epsilon && fst xA /= getAttrOfRestriction nullRestriction
            then trace ("Attempt split added split") $ replaceWithInternal (Leaf n x l table) (fst xA)
            else Leaf n x (majorityLabel table) table
    else Leaf n x (majorityLabel table) table
attemptSplit node = node

replaceWithInternal :: DecisionTree -> Attr -> DecisionTree
replaceWithInternal (Leaf n x l table) attr =
  let split = snd (findBestSplitWithAttr table attr)
      leftAfter = Leaf 0 x l emptyTable
      rightAfter = Leaf 0 x l emptyTable
   in Node n x split (leftAfter, rightAfter) table
replaceWithInternal (Node n x _ _ table) attr =
  let split = snd (findBestSplitWithAttr table attr)
      leftAfter = Leaf 0 x "null" emptyTable
      rightAfter = Leaf 0 x "null" emptyTable
   in Node n x split (leftAfter, rightAfter) table

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

-- Updated majorityLabel function
majorityLabel :: NTable -> Label
majorityLabel table =
  let combinedSums = Map.unionsWith (+) $ Map.elems table
      labels = Map.keys combinedSums
   in if all isNumericOrEmpty labels
        then show (averageDouble (map readNumeric labels))
        else fst $ maximumBy (comparing snd) $ Map.toList combinedSums



classify :: DecisionTree -> Object -> Label
classify (Leaf _ _ c _) _ = c
classify (Node _ _ constraints branches _) obj =
  if satisfiesConstraint constraints obj
    then classify (fst branches) obj
    else classify (snd branches) obj

matchesAttribute :: Restriction -> Feature -> Bool
matchesAttribute (Equal attr _) (AStr featureAttr _) =
  attr == featureAttr
matchesAttribute (Equal attr _) (ADouble featureAttr _) =
  attr == featureAttr
matchesAttribute (Order attr _ _) (ADouble featureAttr _) =
  attr == featureAttr
matchesAttribute (Order attr _ _) (AStr featureAttr _) =
  attr == featureAttr

findBestSplitWithAttr :: NTable -> Attr -> (Double, Restriction)
findBestSplitWithAttr table requiredAttr =
  maximumBy (comparing fst) [(informationGainForRestriction table restriction, restriction) | restriction <- possibleRestrictionsWithAttr table requiredAttr]

allEntriesHaveSameLabel :: NTable -> Bool
allEntriesHaveSameLabel ntable =
  let relevantLabels = concatMap extractRelevantLabels (Map.elems ntable)
   in case relevantLabels of
        [] -> True
        (x : xs) -> all (== x) xs

extractRelevantLabels :: Statistics -> [Label]
extractRelevantLabels stats =
  Map.keys $ Map.filter (> 0) stats

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

informationGainForAttr :: Attr -> NTable -> Double
informationGainForAttr attr nTable =
  let restrictions = possibleRestrictionsWithAttr nTable attr
      gains = [informationGainForRestriction nTable r | r <- restrictions]
   in if not (null gains) then maximum gains else 0.0

informationGainForAttrs :: [Attr] -> NTable -> [(Attr, Double)]
informationGainForAttrs attrs nTable =
  [(attr, informationGainForAttr attr nTable) | attr <- attrs]

totalEntropy :: NTable -> Double
totalEntropy nTable =
  let labelCounts = concatMap Map.elems (Map.elems nTable)
      totalCount = fromIntegral (sum labelCounts)
      probs = map (\count -> let p = fromIntegral count / totalCount in p * logBase 2 p) labelCounts
   in -sum probs

entropyForFeature :: Statistics -> Double
entropyForFeature labelCounts =
  let total = fromIntegral $ sum $ Map.elems labelCounts
      probs = [fromIntegral count / total | count <- Map.elems labelCounts]
   in -sum [p * logBase 2 p | p <- probs, p > 0]

possibleRestrictionsWithAttr :: NTable -> Attr -> [Restriction]
possibleRestrictionsWithAttr table requiredAttr =
  [Order attr Data.LTE value | (ADouble attr value) <- Map.keys table, attr == requiredAttr]
    ++ [Equal attr value | (AStr attr value) <- Map.keys table, attr == requiredAttr]

getNTable :: DecisionTree -> NTable
getNTable (Leaf _ _ _ ntable) = ntable
getNTable (Node _ _ _ _ ntable) = ntable

distinctLabelsCount :: NTable -> Int
distinctLabelsCount nTable =
  let allLabels = concatMap Map.keys (Map.elems nTable)
      uniqueLabels = removeDuplicates allLabels
   in length uniqueLabels

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

enoughDatePoints :: Int -> Bool
enoughDatePoints n = n `mod` 10 == 0
