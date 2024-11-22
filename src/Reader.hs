module Reader where

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delimiter str = go str []
  where
    go [] acc = [reverse acc]
    go (x : xs) acc
      | x == delimiter = reverse acc : go xs []
      | otherwise = go xs (x : acc)

replaceYesNo :: [String] -> [Int]
replaceYesNo = map replace
  where
    replace "Yes" = 1
    replace "No" = 0
    replace s = read s

readFileToListOfLists :: FilePath -> IO ([String], [[Int]])
readFileToListOfLists filePath = do
  contents <- readFile filePath
  let linesOfStrings = lines contents
  let header = splitOn ',' (head linesOfStrings)
  let listOfLists = map (replaceYesNo . splitOn ',') (tail linesOfStrings)
  return (header, listOfLists)
