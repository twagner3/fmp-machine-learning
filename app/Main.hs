module Main where

import Reader
import VFDT

main :: IO ()
main = do
  putStrLn "Implementierung eines Machine-Learning-Systems mit Entscheidungsbaeumen"
  putStrLn "von Tim Wagner"

  labeledObjects <- readObjectsFromFile "data/daten.csv"
  testingObjects <- readObjectsFromFile "data/daten.csv"

  let tree = buildNew (take 20000 labeledObjects)

  let predictions = map (classify tree . fst) (take 20000 testingObjects)

  print tree

  let expected = map snd (take 20000 testingObjects)
  let res = zip expected predictions

  let incorrect = filter (uncurry (/=)) res

  print ("Total number of testing instances: " ++ show (length expected))
  print ("Number of incorrect predictions: " ++ show (length incorrect))

  let accuracy = fromIntegral (length expected - length incorrect) / fromIntegral (length expected)

  print ("Accuracy of the model: " ++ show (accuracy :: Double))
