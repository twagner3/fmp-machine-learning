module Object where

import Constraint

data Feature = AInt Attr Int | AStr Attr String

type Object = [Feature]

instance Show Feature where
  show (AInt attr value) = attr ++ ": " ++ show value
  show (AStr attr value) = attr ++ ": \"" ++ value ++ "\""