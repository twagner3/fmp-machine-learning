module Main where

import Reader

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    result <- readFileToListOfLists "data/example1.csv"
    print result
