module Decision where

giniImpurity :: ((Int, Int), (Int, Int)) -> Double
giniImpurity ((ly, ln), (ry, rn)) =
  let total = fromIntegral (ly + ln + ry + rn)
      countL = fromIntegral (ly + ln)
      countR = fromIntegral (ry + rn)
   in (countL / total) * giniImpurityLeaf (ly, ln) + (countR / total) * giniImpurityLeaf (ry, rn)

giniImpurityLeaf :: (Int, Int) -> Double
giniImpurityLeaf (y, n) =
  let total = fromIntegral (y + n)
   in 1 - (fromIntegral y / total) ** 2 - (fromIntegral n / total) ** 2

--------------

splitValuesOfRow :: Int -> [[Int]] -> [Double]
splitValuesOfRow row xs = splitValues (removeDuplicates (selectRow row xs))

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

--------------

countByThresholdsList :: Int -> [Double] -> [[Int]] -> [((Int, Int), (Int, Int))]
countByThresholdsList row thresholds xs = map (\threshold -> countByThreshold row threshold xs) thresholds

countByThreshold :: Int -> Double -> [[Int]] -> ((Int, Int), (Int, Int))
countByThreshold i threshold xs =
  let aboveThreshold = filter (\row -> fromIntegral (row !! i) > threshold) xs
      belowThreshold = filter (\row -> fromIntegral (row !! i) <= threshold) xs
   in (countLastColumn aboveThreshold, countLastColumn belowThreshold)

countLastColumn :: [[Int]] -> (Int, Int)
countLastColumn =
  foldr
    ( \x (ones, zeros) ->
        if last x == 0
          then (ones, zeros + 1)
          else (ones + 1, zeros)
    )
    (0, 0)

--------------

bestThresholdAndGini :: Int -> [[Int]] -> (Double, Double)
bestThresholdAndGini row table = findMin (thresholdAndGini row table)
  where 
    findMin [] = (0, 1)
    findMin [x] = x
    findMin (x : xs) = let minRest = findMin xs
                       in if snd x < snd minRest then x else minRest

thresholdAndGini :: Int -> [[Int]] -> [(Double, Double)]
thresholdAndGini row xs =
  let thresholds = splitValuesOfRow row xs
      count = countByThresholdsList row thresholds xs
      giniImpurityList = map giniImpurity count
   in zip thresholds giniImpurityList

