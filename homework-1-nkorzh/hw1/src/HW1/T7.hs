module HW1.T7 where

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show)

infixr 5 :+

instance Semigroup (ListPlus a) where
  as <> bs = as `lconcat` bs
    where
      lconcat (Last x) ys = x :+ ys
      lconcat (x :+ xs) ys = x :+ xs `lconcat` ys

data Inclusive a b = This a | That b | Both a b
  deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This a <> This b = This (a <> b)
  This a <> That b = Both a b
  That a <> This b = Both b a
  That a <> That b = That (a <> b)
  This a <> Both leftB rightB = Both (a <> leftB) rightB
  That a <> Both firstB rightB = Both firstB (a <> rightB)
  Both leftA rightA <> This b = Both (leftA <> b) rightA
  Both leftA rightA <> That b = Both leftA (rightA <> b)
  Both leftA rightA <> Both leftB rightB = Both (leftA <> leftB) (rightA <> rightB)

newtype DotString = DS String
  deriving (Show)

instance Semigroup DotString where
  DS a <> DS b
    | null a = DS b
    | null b = DS a
    | otherwise = DS (a <> "." <> b)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F a <> F b = F (a . b)

instance Monoid (Fun a) where
  mempty = F id
