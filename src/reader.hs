module Reader where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter str = go str []
  where
    go [] acc = [reverse acc]
    go (x:xs) acc
      | x == delimiter = reverse acc : go xs []
      | otherwise = go xs (x:acc)

readFileToListOfLists :: FilePath -> IO [[String]]
readFileToListOfLists filePath = do
    contents <- readFile filePath
    let linesOfStrings = lines contents
    let listOfLists = map (splitOn ',') linesOfStrings
    return listOfLists