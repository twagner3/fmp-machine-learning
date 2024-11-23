module Tree where

type Attr = String

data Feature = AInt Attr Int | AStr Attr String

type Object = [Feature]

data Ordering = LT | GT | EQ

data Restriction = Equal Attr String | Order Attr Tree.Ordering Int

type Constraints = [Restriction]

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

instance Show Tree.Ordering where
  show Tree.LT = "<"
  show Tree.GT = ">"
  show Tree.EQ = "="

instance Show Restriction where
  show (Equal attr val) = attr ++ " = " ++ show val
  show (Order attr ord val) = attr ++ " " ++ show ord ++ " " ++ show val

myTree :: DecisionTree
myTree =
  Node
    [ ( [Order "age" Tree.LT 30, Equal "gender" "female"],
        Node
          [ ([Order "age" Tree.LT 21], Leaf 0),
            ([Order "age" Tree.GT 20], Leaf 1)
          ]
      ),
      ( [Order "age" Tree.LT 50],
        Node
          [ ([Equal "gender" "male"], Leaf 2),
            ([Equal "gender" "female"], Leaf 3)
          ]
      ),
      ([Order "age" Tree.LT 51, Order "age" Tree.GT 29], Leaf 4)
    ]