module HW2.T2
  ( -- dist
    distAnnotated,
    distExcept,
    distFun,
    distList,
    distOption,
    distPair,
    distPrioritised,
    distQuad,
    distStream,
    -- wrap
    wrapAnnotated,
    wrapExcept,
    wrapFun,
    wrapList,
    wrapOption,
    wrapPair,
    wrapPrioritised,
    wrapQuad,
    wrapStream,
    flattenList,
  )
where

import HW2.T1
  ( Annotated (..),
    Except (..),
    Fun (..),
    List (..),
    Option (..),
    Pair (..),
    Prioritised (..),
    Quad (..),
    Stream (..),
  )
import Prelude (Monoid, Semigroup, flip, mempty, ($), (<>))

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption (_, _) = None

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P al ar, P bl br) = P (al, bl) (ar, br)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# e1 <> e2

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b) = High (a, b)
distPrioritised (High a, Medium b) = High (a, b)
distPrioritised (High a, Low b) = High (a, b)
distPrioritised (Medium a, High b) = High (a, b)
distPrioritised (Low a, High b) = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b) = Medium (a, b)
distPrioritised (Low a, Medium b) = Medium (a, b)
distPrioritised (Low a, Low b) = Low (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> as, b :> bs) = (a, b) :> distStream (as, bs)

flattenList :: List (List a) -> List a
flattenList = go
  where
    listAdd :: List a -> List a -> List a
    listAdd Nil ys = ys
    listAdd (x :. xs) ys = x :. listAdd xs ys

    go Nil = Nil
    go (y :. ys) = y `listAdd` go ys

distList :: (List a, List b) -> List (a, b)
distList (a :. as, b :. bs) =
  let makePairs :: x -> List y -> List (x, y)
      makePairs _ Nil = Nil
      makePairs x (y :. ys) = (x, y) :. makePairs x ys

      mapList :: (x -> y) -> List x -> List y
      mapList _ Nil = Nil
      mapList f (x :. xs) = (f x) :. mapList f xs
   in flattenList $ mapList (flip makePairs (b :. bs)) (a :. as)
distList (Nil, _) = Nil
distList (_, Nil) = Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F fToA, F fToB) = F $ \i -> (fToA i, fToB i)

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept :: a -> Except e a
wrapExcept a = Success a

wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = Low a

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList :: a -> List a
wrapList = flip (:.) Nil

wrapFun :: a -> Fun i a
wrapFun a = F $ \_ -> a
