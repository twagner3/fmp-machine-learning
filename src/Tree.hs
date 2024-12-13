module Tree where

import Constraint
import Data.Foldable (find)
import Object

type Class = Int

data DecisionTree = Node [(Constraints, DecisionTree)] | Leaf Class

instance Show DecisionTree where
  show = showTree 0
    where
      showTree :: Int -> DecisionTree -> String
      showTree depth (Leaf cls) = indent depth ++ "Leaf " ++ show cls ++ "\n"
      showTree depth (Node branches) =
        indent depth ++ "Node\n" ++ concatMap (showBranch depth) branches

      showBranch :: Int -> (Constraints, DecisionTree) -> String
      showBranch depth (constraints, subtree) =
        indent (depth + 1) ++ "Branch " ++ show constraints ++ "\n" ++ showTree (depth + 2) subtree

      indent :: Int -> String
      indent n = replicate (n * 2) ' '

myTree :: DecisionTree
myTree =
  Node
    [ ( [Order "age" Constraint.LT 30, Equal "gender" "female"],
        Node
          [ ([Order "age" Constraint.LT 21], Leaf 0),
            ([Order "age" Constraint.GT 20], Leaf 1)
          ]
      ),
      ( [Order "age" Constraint.LT 50],
        Node
          [ ([Equal "gender" "male"], Leaf 2),
            ([Equal "gender" "female"], Leaf 3)
          ]
      ),
      ([Order "age" Constraint.LT 51, Order "age" Constraint.GT 29], Leaf 4)
    ]

---------------------------------------------------

classify :: DecisionTree -> Object -> [Class]
classify (Leaf c) _ = [c]
classify (Node branches) obj =
  concatMap
    ( \(constraints, subtree) ->
        if satisfies constraints obj
          then classify subtree obj
          else []
    )
    branches

satisfies :: Constraints -> Object -> Bool
satisfies constraints obj = all (`satisfiesConstraint` obj) constraints

satisfiesConstraint :: Restriction -> Object -> Bool
satisfiesConstraint (Equal attr val) obj =
  case lookupAttr attr obj of
    Just (AStr _ strVal) -> strVal == val
    _ -> True
satisfiesConstraint (Order attr ord val) obj =
  case lookupAttr attr obj of
    Just (AInt _ intVal) -> compareWithOrdering ord intVal val
    _ -> True

lookupAttr :: Attr -> Object -> Maybe Feature
lookupAttr attr = find (\feature -> getAttr feature == attr)

getAttr :: Feature -> Attr
getAttr (AInt attr _) = attr
getAttr (AStr attr _) = attr

compareWithOrdering :: Constraint.Ordering -> Int -> Int -> Bool
compareWithOrdering Constraint.LT x y = x < y
compareWithOrdering Constraint.GT x y = x > y
compareWithOrdering Constraint.EQ x y = x == y

------------------------------------------------
