module HW0.T4 where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a] -- behaves like Data.List.repeat
repeat' a = fix (a :)

map' :: (a -> b) -> [a] -> [b] -- behaves like Data.List.map
map' =
  fix
    ( \rec f l ->
        if null l
          then []
          else f (head l) : rec f (tail l)
    )

fib :: Natural -> Natural -- computes the n-th Fibonacci number
fib =
  fix
    ( \rec n ->
        if n == 0
          then 0
          else
            if n == 1 || n == 2
              then 1
              else rec (n - 2) + rec (n - 1)
    )

fac :: Natural -> Natural -- computes the factorial
fac = fix (\rec n -> if n == 0 then 1 else n * rec (n - 1))
