module Main where

import Data (LabeledObject)
import Reader
import VFDT

main :: IO ()
main = do
  putStrLn "Implementierung eines Machine-Learning-Systems mit Entscheidungsbaeumen"
  putStrLn "von Tim Wagner"

  labeledObjects <- readObjectsFromFile "data/daten.csv"

  let tree = buildNew (take 1 labeledObjects)

  processBatches tree labeledObjects 50

processBatches :: DecisionTree -> [LabeledObject] -> Int -> IO ()
processBatches _ [] _ = putStrLn "Verarbeitung abgeschlossen."
processBatches tree objects batchSize = do
  let (currentBatch, remainingObjects) = splitAt batchSize objects

  let updatedTree = foldl learn tree currentBatch

  let acc = accuracy updatedTree currentBatch
  putStrLn $ "Aktuelle Genauigkeit: " ++ show acc

  appendFile "accuracy_log.txt" (show acc ++ "\n")

  processBatches updatedTree remainingObjects batchSize

accuracy :: DecisionTree -> [LabeledObject] -> Double
accuracy tree objects =
  let predictions = map (classify tree . fst) objects
      expected = map snd objects
      res = zip expected predictions
      incorrect = filter (uncurry (/=)) res
   in fromIntegral (length expected - length incorrect) / fromIntegral (length expected)
