module HW2.T1
  ( Annotated (..),
    Except (..),
    Fun (..),
    List (..),
    Option (..),
    Pair (..),
    Prioritised (..),
    Quad (..),
    Stream (..),
    Tree (..),
    mapAnnotated,
    mapExcept,
    mapFun,
    mapList,
    mapOption,
    mapPair,
    mapPrioritised,
    mapQuad,
    mapStream,
    mapTree,
  )
where

data Option a = None | Some a

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f (Some a) = Some (f a)
mapOption _ None = None

data Pair a = P a a

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P left right) = P (f left) (f right)

data Quad a = Q a a a a

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

data Annotated e a = a :# e
  deriving (Show)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = (f a) :# e

infix 0 :#

data Except e a = Error e | Success a
  deriving (Show)

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Success a) = Success (f a)
mapExcept _ (Error e) = Error e

data Prioritised a = Low a | Medium a | High a

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a) = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a) = High (f a)

data Stream a = a :> Stream a

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (x :> xs) = f x :> mapStream f xs

infixr 5 :>

data List a = Nil | a :. List a

mapList :: (a -> b) -> (List a -> List b)
mapList f (x :. xs) = f x :. mapList f xs
mapList _ Nil = Nil

infixr 5 :.

data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F (f . g)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Branch leftBr self rightBr) = Branch (mapTree f leftBr) (f self) (mapTree f rightBr)
mapTree _ Leaf = Leaf
