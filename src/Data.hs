module Data where

import Data.Foldable (find)
import Data.List (nub)

type Attr = String

-----------------------------

data Ordering = LT | GT | EQ | LTE | GTE

instance Eq Data.Ordering where
  Data.LT == Data.LT = True
  Data.GT == Data.GT = True
  Data.EQ == Data.EQ = True
  Data.LTE == Data.LTE = True
  Data.GTE == Data.GTE = True
  _ == _ = False


data Restriction = Equal Attr String | Order Attr Data.Ordering Double

-- Defines equality for `Restriction`, comparing attributes, operators, and values.
instance Eq Restriction where
  (Equal attr1 str1) == (Equal attr2 str2) =
    attr1 == attr2 && str1 == str2
  (Order attr1 ord1 dbl1) == (Order attr2 ord2 dbl2) =
    attr1 == attr2 && ord1 == ord2 && dbl1 == dbl2
  _ == _ = False

type Constraints = [Restriction]

instance Show Data.Ordering where
  show Data.LT = "<"
  show Data.GT = ">"
  show Data.EQ = "="
  show Data.LTE = "<="
  show Data.GTE = ">="

instance Show Restriction where
  show (Equal attr val) = attr ++ " = " ++ show val
  show (Order attr ord val) = attr ++ " " ++ show ord ++ " " ++ show val

------------------------------

data Feature = ADouble Attr Double | AStr Attr String

-- Defines equality for `Feature`, comparing attributes and their values.
instance Eq Feature where
  (ADouble attr1 value1) == (ADouble attr2 value2) = attr1 == attr2 && value1 == value2
  (AStr attr1 value1) == (AStr attr2 value2) = attr1 == attr2 && value1 == value2
  _ == _ = False

-- Defines ordering for `Feature`, primarily comparing attributes and secondarily their values.
instance Ord Feature where
    compare (ADouble attr1 val1) (ADouble attr2 val2) =
        compare attr1 attr2 <> compare val1 val2
    compare (AStr attr1 str1) (AStr attr2 str2) =
        compare attr1 attr2 <> compare str1 str2
    compare (ADouble _ _) (AStr _ _) = Prelude.LT
    compare (AStr _ _) (ADouble _ _) = Prelude.GT  

type Object = [Feature]
type Label = String
type LabeledObject = (Object, Label)

getAttr :: Object -> [Attr]
getAttr object = [attr | feature <- object, let attr = extractAttr feature]

extractAttr :: Feature -> Attr
extractAttr (ADouble attr _) = attr
extractAttr (AStr attr _) = attr

instance Show Feature where
  show (ADouble attr value) = attr ++ ": " ++ show value
  show (AStr attr value) = attr ++ ": \"" ++ value ++ "\""

-- Checks whether an object satisfies a list of constraints.
satisfies :: Constraints -> Object -> Bool
satisfies constraints obj = all (`satisfiesConstraint` obj) constraints

-- Checks if a single restriction is satisfied by an object.
satisfiesConstraint :: Restriction -> Object -> Bool
satisfiesConstraint (Equal attr val) obj =
  case lookupAttr attr obj of
    Just (AStr _ strVal) -> strVal == val
    _ -> True
satisfiesConstraint (Order attr ord val) obj =
  case lookupAttr attr obj of
    Just (ADouble _ doubleVal) -> compareWithOrdering ord doubleVal val
    _ -> True

-- Finds a feature in an object by its attribute name.
lookupAttr :: Attr -> Object -> Maybe Feature
lookupAttr attr = find (\feature -> extractAttr feature == attr)

-- Compares two doubles using a given ordering operator.
compareWithOrdering :: Data.Ordering -> Double -> Double -> Bool
compareWithOrdering Data.LT x y = x < y
compareWithOrdering Data.GT x y = x > y
compareWithOrdering Data.EQ x y = x == y
compareWithOrdering Data.LTE x y = x <= y
compareWithOrdering Data.GTE x y = x >= y

-- Splits a table of labeled objects into two parts based on a restriction.
splitTable :: [LabeledObject] -> Restriction -> ([LabeledObject], [LabeledObject])
splitTable table restriction =
  let left = filter (not . satisfiesConstraint restriction . fst) table
      right = filter (satisfiesConstraint restriction . fst) table
   in (left, right)

-- Extracts the label from a labeled object.
classOf :: LabeledObject -> Label
classOf = snd

-- Extracts all unique attributes from a table of labeled objects.
extractUniqueAttrs :: [LabeledObject] -> [Attr]
extractUniqueAttrs labeledObjects =
  nub [attr | (object, _) <- labeledObjects, attr <- getAttr object]

-- Retrieves the attribute involved in a restriction.
getAttrOfRestriction :: Restriction -> Attr
getAttrOfRestriction (Equal attr _) = attr
getAttrOfRestriction (Order attr _ _) = attr

-- Removes a specific attribute from a list of attributes.
removeAttr :: [Attr] -> Attr -> [Attr]
removeAttr attrs target = filter (/= target) attrs
