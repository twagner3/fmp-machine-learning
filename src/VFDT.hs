module VFDT where

import Data
import Data.List (maximumBy)
import Data.Map qualified as Map
import Data.Ord (comparing)

data DecisionTree = Leaf Label NTable | Node Restriction (DecisionTree, DecisionTree) NTable
  deriving (Show)

type Statistics = Map.Map Label Int

type NTable = Map.Map Feature Statistics

----------------------------------------

emptyTable :: NTable
emptyTable = Map.empty

initTree :: DecisionTree
initTree = Leaf "default" emptyTable

giniIndex :: Statistics -> Double
giniIndex counts = 1 - sum [(fromIntegral count / total) ^ (2 :: Integer) | count <- Map.elems counts]
  where
    total = fromIntegral $ sum $ Map.elems counts

hoeffdingBound :: Double -> Double -> Double -> Double
hoeffdingBound r n delta = sqrt ((r * r * log (1 / delta)) / (2 * n))

splitNTable :: NTable -> Restriction -> (NTable, NTable)
splitNTable nTable restriction =
  Map.partitionWithKey (\feature _ -> satisfiesConstraint restriction [feature]) nTable

splitEntropy :: NTable -> Restriction -> Double
splitEntropy nTable restriction =
  let (leftTable, rightTable) = splitNTable nTable restriction
      totalExamples = fromIntegral $ sum [sum $ Map.elems labelCounts | labelCounts <- Map.elems nTable]
      leftWeight = fromIntegral (sum [sum $ Map.elems labelCounts | labelCounts <- Map.elems leftTable]) / totalExamples
      rightWeight = fromIntegral (sum [sum $ Map.elems labelCounts | labelCounts <- Map.elems rightTable]) / totalExamples
      leftEntropy = totalEntropy leftTable
      rightEntropy = totalEntropy rightTable
   in leftWeight * leftEntropy + rightWeight * rightEntropy

entropyForFeature :: Statistics -> Double
entropyForFeature labelCounts =
  let total = fromIntegral $ sum $ Map.elems labelCounts
      probs = [fromIntegral count / total | count <- Map.elems labelCounts]
   in -sum [p * logBase 2 p | p <- probs, p > 0]

totalEntropy :: NTable -> Double
totalEntropy nTable =
  let totalExamples = sum [sum $ Map.elems labelCounts | labelCounts <- Map.elems nTable]
      featureEntropies =
        [ (fromIntegral (sum $ Map.elems labelCounts) / fromIntegral totalExamples) * entropyForFeature labelCounts
          | labelCounts <- Map.elems nTable
        ]
   in sum featureEntropies

incrementInNTable :: LabeledObject -> NTable -> NTable
incrementInNTable (object, objLabel) nTable = foldl (updateFeature objLabel) nTable object
  where
    updateFeature :: Label -> NTable -> Feature -> NTable
    updateFeature label table feature = Map.insertWith (Map.unionWith (+)) feature (Map.singleton label 1) table

incrementStatistic :: Label -> Statistics -> Statistics
incrementStatistic label = Map.insertWith (+) label 1

buildMultiple :: [LabeledObject] -> DecisionTree
buildMultiple [] = initTree
buildMultiple (firstObject : restObjects) =
  foldl learn initialTree restObjects
  where
    initialTree = learn initTree firstObject

build :: LabeledObject -> DecisionTree
build = learn initTree

learn :: DecisionTree -> LabeledObject -> DecisionTree
learn (Node restriction children table) object =
  learnNext (reEvalSplit (Node restriction children (incrementInNTable object table))) object
learn (Leaf _ table) object =
  learnNext (attemptSplit (Leaf (majorityLabel table) (incrementInNTable object table))) object

learnNext :: DecisionTree -> LabeledObject -> DecisionTree
learnNext (Node restriction (left, right) table) object =
  if satisfiesConstraint restriction (fst object)
    then Node restriction (learn left object, right) table
    else Node restriction (left, learn right object) table
learnNext (Leaf label table) _ =
  Leaf label table

reEvalSplit :: DecisionTree -> DecisionTree
reEvalSplit (Node restriction (left, right) table) =
  let bestSplit = findBestSplit table
      currentSplitQuality = splitEntropy table restriction
      epsilon = hoeffdingBound 1 (fromIntegral $ totalExamples table) 0.05
   in if fst bestSplit - currentSplitQuality > epsilon
        then
          if snd bestSplit == nullRestriction
            then Leaf (majorityLabel table) table
            else
              let (leftTable, rightTable) = splitNTable table (snd bestSplit)
               in Node (snd bestSplit) (Leaf (majorityLabel leftTable) leftTable, Leaf (majorityLabel rightTable) rightTable) table
        else Node restriction (left, right) table
reEvalSplit leaf = leaf

attemptSplit :: DecisionTree -> DecisionTree
attemptSplit (Leaf label table) =
  let bestSplit = findBestSplit table
      epsilon = hoeffdingBound 1 (fromIntegral $ totalExamples table) 0.05
   in if fst bestSplit > epsilon
        then
          let (leftTable, rightTable) = splitNTable table (snd bestSplit)
           in Node (snd bestSplit) (Leaf (majorityLabel leftTable) leftTable, Leaf (majorityLabel rightTable) rightTable) table
        else Leaf label table
attemptSplit node = node

findBestSplit :: NTable -> (Double, Restriction)
findBestSplit table =
  maximumBy (comparing fst) [(splitEntropy table restriction, restriction) | restriction <- possibleRestrictions table]

possibleRestrictions :: NTable -> [Restriction]
possibleRestrictions table =
  [Order attr Data.LT value | (ADouble attr _, _) <- Map.toList table, value <- [0.0, 0.1 .. 1.0]]
    ++ [Equal attr value | (AStr attr _, stats) <- Map.toList table, value <- Map.keys stats]

nullRestriction :: Restriction
nullRestriction = Order "null" Data.LT 0

majorityLabel :: NTable -> Label
majorityLabel table =
  fst $ maximumBy (comparing snd) $ Map.toList $ Map.unionsWith (+) $ Map.elems table

totalExamples :: NTable -> Int
totalExamples table = sum [sum $ Map.elems labelCounts | labelCounts <- Map.elems table]

classify :: DecisionTree -> Object -> Label
classify (Leaf c _) _ = c
classify (Node constraints branches _) obj =
  if satisfiesConstraint constraints obj
    then classify (fst branches) obj
    else classify (snd branches) obj
   