module Main where

import Reader
import VFDT

main :: IO ()
main = do
  putStrLn "Implementierung eines Machine-Learning-Systems mit Entscheidungsbaeumen"
  putStrLn "von Tim Wagner"

  labeledObjects <- readObjectsFromFile "data/exampleTraining2.csv"
  testingObjects <- readObjectsFromFile "data/exampleTesting2.csv"

  let tree = buildMultiple (take 225 labeledObjects)

  let predictions = map (classify tree . fst) testingObjects

  let expected = map snd testingObjects
  let res = zip expected predictions

  let incorrect = filter (uncurry (/=)) res

  print ("Total number of testing instances: " ++ show (length expected))
  print ("Number of incorrect predictions: " ++ show (length incorrect))

  let accuracy = fromIntegral (length expected - length incorrect) / fromIntegral (length expected)

  print ("Accuracy of the model: " ++ show (accuracy :: Double))
