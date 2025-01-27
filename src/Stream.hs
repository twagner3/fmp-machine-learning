module Stream where

import Data
import System.Random (randomRIO)

-- List of German states
bundeslaender :: [String]
bundeslaender =
  [ "Baden-Württemberg",
    "Bayern",
    "Berlin",
    "Brandenburg",
    "Bremen",
    "Hamburg",
    "Hessen",
    "Mecklenburg-Vorpommern",
    "Niedersachsen",
    "Nordrhein-Westfalen",
    "Rheinland-Pfalz",
    "Saarland",
    "Sachsen",
    "Sachsen-Anhalt",
    "Schleswig-Holstein",
    "Thüringen"
  ]

-- Function to generate a random list of ages
generateRandomAges :: Int -> IO [Int]
generateRandomAges n = mapM (const (randomRIO (1, 99))) [1 .. n]

-- Function to generate a random list of states
generateRandomStates :: Int -> IO [String]
generateRandomStates n =
  mapM (const (randomRIO (0, length bundeslaender - 1))) [1 .. n]
    >>= return . map (bundeslaender !!)
  

-- Function to generate random data (ages and states) as a list of tuples
generateRandomData :: Int -> IO [(Int, String)]
generateRandomData n = do
  ages <- generateRandomAges n
  states <- generateRandomStates n
  return (zip ages states)

-- Generate a single phase of data with specified car ownership rules
generatePhaseData :: Int -> (Int -> String -> Bool) -> IO [LabeledObject]
generatePhaseData n carOwnershipRule = do
  randomData <- generateRandomData n
  return [createLabeledObject age state (carOwnershipRule age state) | (age, state) <- randomData]

-- Define car ownership rules for each phase
phase1Rule :: Int -> String -> Bool
phase1Rule age state = age > 21 && state `notElem` ["Bremen", "Berlin", "Hamburg"]

phase2Rule :: Int -> String -> Bool
phase2Rule age state = age > 18 && age < 40 && state `notElem` ["Bremen", "Berlin", "Hamburg"]

phase3Rule :: Int -> String -> Bool
phase3Rule age state = age > 18 && state `elem` ["Bremen", "Berlin", "Hamburg"]

-- Generate infinite data with consistent drift intervals
generateInfiniteData :: Int -> IO [LabeledObject]
generateInfiniteData n = do
  phase1Data <- generatePhaseData n phase1Rule
  phase2Data <- generatePhaseData n phase2Rule
  phase3Data <- generatePhaseData n phase3Rule
  let phases = cycle [phase1Data, phase2Data, phase3Data]
  return (concat phases)

-- Function to create LabeledObject from age, state, and car ownership status
createLabeledObject :: Int -> String -> Bool -> LabeledObject
createLabeledObject age state hasCar =
  ([ADouble "Alter" (fromIntegral age), AStr "Bundesland" state], if hasCar then "Hat Auto" else "Kein Auto")