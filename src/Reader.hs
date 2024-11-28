module Reader where

import Object
import Attribute
import Text.Read (readMaybe)

split :: String -> [String]
split str = go str []
  where
    go [] acc = [reverse acc]
    go (x : xs) acc
      | x == ',' = reverse acc : go xs []
      | otherwise = go xs (x : acc)


readFileToListOfLists :: FilePath -> IO [Object]
readFileToListOfLists filePath = do
  contents <- readFile filePath
  let linesOfStrings = lines contents
  let header = split (head linesOfStrings) :: [Attr]
  let values = map split (tail linesOfStrings)
  let objects = map (convertRowToObject header) values
  return objects

convertRowToObject :: [Attr] -> [String] -> Object
convertRowToObject = zipWith convertValue

convertValue :: Attr -> String -> Feature
convertValue attr value =
  case readMaybe value :: Maybe Int of
    Just intVal -> AInt attr intVal
    Nothing     -> AStr attr value
