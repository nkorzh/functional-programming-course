module HW0.T5 where

import GHC.Num (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

ns :: Nat a -> Nat a
ns n f a = f (n f a)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus fL sL fR sR = fL fR (sL fR sR)
nmult fL sL fR = fL (sL fR)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns (nFromNatural (n - 1))

nToNum :: Num a => Nat a -> a
nToNum a = a (+ 1) 0
