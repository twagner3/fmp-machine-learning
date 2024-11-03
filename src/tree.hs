module Tree where

type Attribute = String

type Threshold = Double

type NumTrue = Int

type NumFalse = Int

data Tree = Leaf NumTrue NumFalse | Node Attribute Threshold Tree Tree