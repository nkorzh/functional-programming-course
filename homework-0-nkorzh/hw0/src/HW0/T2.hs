module HW0.T2 where

import Data.Void (Void(..), absurd)


type Not a = a -> Void

doubleNeg :: a -> Not (Not a) -- a -> (a -> Void) -> Void
doubleNeg a b = b a

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg = (. doubleNeg)

