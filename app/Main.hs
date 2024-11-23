module Main where

import Reader
import Tree

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  result <- readFileToListOfLists "data/example1.csv"
  print result

  let tree = myTree
  print tree
