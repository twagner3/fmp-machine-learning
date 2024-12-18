module Reader where

import Data
import Text.Read (readMaybe)

split :: String -> [String]
split str = go str []
  where
    go [] acc = [reverse acc]
    go (x : xs) acc
      | x == ',' = reverse acc : go xs []
      | otherwise = go xs (x : acc)

readObjectsFromFile :: FilePath -> IO [LabeledObject]
readObjectsFromFile filePath = do
  contents <- readFile filePath
  let linesOfFile = lines contents
  let header = split (head linesOfFile) :: [Attr]
  let values = map split (tail linesOfFile)
  let objects = map (convertRowToLabeledObject header) values
  return objects

convertRowToLabeledObject :: [Attr] -> [String] -> LabeledObject
convertRowToLabeledObject header values =
  let objectValues = init values
      label = last values
      object = convertRowToObject header objectValues
   in (object, label)

convertRowToObject :: [Attr] -> [String] -> Object
convertRowToObject = zipWith convertValue

convertValue :: Attr -> String -> Feature
convertValue attr value =
  case readMaybe value :: Maybe Double of
    Just doubleVal -> ADouble attr doubleVal
    Nothing -> AStr attr value
