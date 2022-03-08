module HW1.T2 where

import Numeric.Natural

data N = Z | S N
  deriving (Show)

nplus :: N -> N -> N -- addition
nplus (S a) b = nplus a (S b)
nplus _ b = b

nmult :: N -> N -> N -- multiplication
nmult Z _ = Z
nmult _ Z = Z
nmult (S a) b = nplus b $ nmult a b

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)
nsub a Z = Just a
nsub Z (S _) = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp a b =
  let subRes = nsub a b
   in case subRes of
        Just Z -> EQ
        Just _ -> GT
        _ -> LT

addNatural :: N -> Natural -> N
addNatural num 0 = num
addNatural peano natural = addNatural (S peano) (natural - 1)

nFromNatural :: Natural -> N
nFromNatural = addNatural Z

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = 1 + nToNum n

nEven :: N -> Bool -- parity checking
nOdd :: N -> Bool -- parity checking
nEven Z = True
nEven (S Z) = False
nEven (S n) = nOdd n

nOdd Z = False
nOdd (S Z) = True
nOdd (S n) = nEven n

divrec :: N -> Maybe N -> N -> N -- tempRes a b
divrec Z Nothing _ = error "can't divide Nothing"
divrec (S tres) Nothing _ = tres
divrec tres (Just Z) _ = tres
divrec tres (Just a) b = divrec (S tres) (nsub a b) b

ndiv :: N -> N -> N -- integer division
ndiv _ Z = error "division by zero"
ndiv a b = divrec Z (Just a) b

nmod :: N -> N -> N -- modulo operation
nmod _ Z = error "division by zero"
nmod a b =
  let maybeNextSub = nsub a b
   in case maybeNextSub of
        Just nextSub -> nmod nextSub b
        _ -> a
