module HW1.T4 where

import HW1.T3 (Tree (..))

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ z Leaf = z
tfoldr f z (Branch _ left self right) = tfoldr f (f self rightSeed) left
  where
    rightSeed = tfoldr f z right

treeToList :: Tree a -> [a] -- output list is sorted
treeToList = tfoldr (:) []
