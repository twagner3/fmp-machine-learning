module Decision where

import Data.List ()
import Tree

giniImpurity :: Tree -> Double
giniImpurity (Leaf yes no) =
  let total = fromIntegral (totalNum (Leaf yes no))
   in 1 - (fromIntegral yes / total) ** 2 - (fromIntegral no / total) ** 2
giniImpurity (Node _ _ left right) =
  let total = fromIntegral (totalNum (Node 0 0 left right))
   in (fromIntegral (totalNum left) / total) * giniImpurity left + (fromIntegral (totalNum right) / total) * giniImpurity right

totalNum :: Tree -> Int
totalNum (Leaf yes no) = yes + no
totalNum (Node _ _ left right) = totalNum left + totalNum right

selectRow :: Int -> [[Int]] -> [Int]
selectRow _ [] = []
selectRow i (x : xs) = x !! i : selectRow i xs

removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates (x : xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

splitValues :: [Int] -> [Double]
splitValues [] = []
splitValues [_] = []
splitValues (x : xs : xss) = (fromIntegral x + fromIntegral xs) / 2 : splitValues (xs : xss)

splitValuesOfRow :: Int -> [[Int]] -> [Double]
splitValuesOfRow i xs = splitValues (removeDuplicates (selectRow i xs))