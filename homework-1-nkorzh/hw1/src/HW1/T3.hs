module HW1.T3 where

-- Pair at Branch stores (size, depth) respectively
data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) 
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (size, _) _ _ _) = size

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left value right = Branch (sumsize, depth) left value right
  where
    sumsize = tsize left + tsize right + 1
    depth = max (tdepth left) (tdepth right) + 1

-- | Depth of the tree, cached.
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, depth) _ _ _) = depth

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember el (Branch _ left value right)
  | el == value = True
  | el > value = tmember el right
  | otherwise = tmember el left

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert el Leaf = mkBranch Leaf el Leaf
tinsert el node@(Branch _ left value right)
  | el == value = node
  | el > value = balance $ mkBranch left value (tinsert el right)
  | otherwise = balance $ mkBranch (tinsert el left) value right

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf -- trying to save some recursion with left fold

-- | Getters for Tree
leftBranch :: Tree a -> Tree a
leftBranch Leaf = Leaf
leftBranch (Branch _ left _ _) = left

rightBranch :: Tree a -> Tree a
rightBranch Leaf = Leaf
rightBranch (Branch _ _ _ right) = right

getValue :: Tree a -> a
getValue Leaf = error "no value at Leaf"
getValue (Branch _ _ value _) = value

-- | Tree balancing helpers
rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = error "can't rotate Leaf left!"
rotateLeft (Branch _ left value right) =
  mkBranch
    (mkBranch left value (leftBranch right))
    (getValue right)
    (rightBranch right)

rotateRight :: Tree a -> Tree a
rotateRight Leaf = error "can't rotate Leaf right!"
rotateRight (Branch _ left value right) =
  mkBranch
    (leftBranch left)
    (getValue left)
    (mkBranch (rightBranch left) value right)

bFactor :: Tree a -> Int
bFactor Leaf = 0
bFactor (Branch _ left _ right) = tdepth right - tdepth left

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance node@(Branch _ left value right) =
  case (bFactor node) of
    2 ->
      if bFactor right < 0
        then rotateLeft $ mkBranch left value (rotateRight right)
        else rotateLeft node
    -2 ->
      if bFactor left > 0
        then rotateRight $ mkBranch (rotateLeft left) value right
        else rotateRight node
    _ -> node
