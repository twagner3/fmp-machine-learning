module Constraint where

type Attr = String

data Ordering = LT | GT | EQ

data Restriction = Equal Attr String | Order Attr Constraint.Ordering Int

type Constraints = [Restriction]

instance Show Constraint.Ordering where
  show Constraint.LT = "<"
  show Constraint.GT = ">"
  show Constraint.EQ = "="

instance Show Restriction where
  show (Equal attr val) = attr ++ " = " ++ show val
  show (Order attr ord val) = attr ++ " " ++ show ord ++ " " ++ show val
