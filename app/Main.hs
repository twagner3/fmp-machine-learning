module Main where

import Reader
import Tree

main :: IO ()
main = do
  putStrLn "Implementierung eines Machine-Learning-Systems mit Entscheidungsbaeumen"
  putStrLn "von Tim Wagner"

  objects <- readObjectsFromFile "data/example1.csv"
  print objects

  let tree = myTree
  print tree
