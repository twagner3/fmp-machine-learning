module Reader where

import Data
import Text.Read (readMaybe)

-- Splits a string by commas into a list of substrings
split :: String -> [String]
split str = go str []
  where
    go [] acc = [reverse acc]
    go (x : xs) acc
      | x == ',' = reverse acc : go xs []
      | otherwise = go xs (x : acc)

-- Reads labeled objects from a CSV file
readObjectsFromFile :: FilePath -> IO [LabeledObject]
readObjectsFromFile filePath = do
  contents <- readFile filePath
  let linesOfFile = lines contents
  let header = split (head linesOfFile) :: [Attr]
  let values = map split (tail linesOfFile)
  let objects = map (convertRowToLabeledObject header) values
  return objects

-- Converts a single row of values into a LabeledObject
convertRowToLabeledObject :: [Attr] -> [String] -> LabeledObject
convertRowToLabeledObject header values =
  let objectValues = init values
      label = last values
      object = convertRowToObject header objectValues
   in (object, label)

-- Converts a list of values into an Object using the corresponding attribute names.
convertRowToObject :: [Attr] -> [String] -> Object
convertRowToObject = zipWith convertValue

-- Converts a single value into a Feature by determining if it is numeric or a string.
convertValue :: Attr -> String -> Feature
convertValue attr value =
  case readMaybe value :: Maybe Double of
    Just doubleVal -> ADouble attr doubleVal
    Nothing -> AStr attr value
