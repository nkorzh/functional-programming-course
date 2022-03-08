module HW2.T3
  ( joinAnnotated,
    joinExcept,
    joinFun,
    joinList,
    joinOption,
  )
where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..))
import HW2.T2 (flattenList)

joinOption :: Option (Option a) -> Option a
joinOption (Some (Some a)) = Some a
joinOption _ = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success (Success a)) = Success a
joinExcept (Success (Error e)) = Error e
joinExcept (Error e) = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1)

joinList :: List (List a) -> List a
joinList = flattenList

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F iToFun) = F flat
  where
    flat i = innerF i
      where
        (F innerF) = iToFun i
