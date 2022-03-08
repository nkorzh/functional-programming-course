{-# LANGUAGE InstanceSigs #-}

module HW2.T4
  ( Expr (..),
    Prim (..),
    State (..),
    eval,
    joinState,
    mapState,
    modifyState,
    wrapState,
  )
where

import qualified Control.Monad
import HW2.T1 (Annotated (..), mapAnnotated)

data State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f (S runState) = S ((mapAnnotated f) . runState)

wrapState :: a -> State s a
wrapState a = S (\s -> a :# s)

joinState :: State s (State s a) -> State s a
joinState (S runState) = S $ \s ->
  let (S inner) :# a = runState s
   in inner a

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

{- Task 3 -}
data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum
  deriving (Show)

data Expr = Val Double | Op (Prim Expr)
  deriving (Show)

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  (/) x y = Op (Div x y)

  fromRational :: Rational -> Expr
  fromRational = Val . fromRational

doEval :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Expr -> Expr -> State [Prim Double] Double
doEval opName f x y = do
  ex <- eval x
  ey <- eval y
  modifyState ((++) [opName ex ey])
  return (ex `f` ey)

doEvalSingle :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> State [Prim Double] Double
doEvalSingle opName f x = do
  ex <- eval x
  modifyState ((++) [opName ex])
  return $ f ex

eval :: Expr -> State [Prim Double] Double
eval (Val num) = return num
eval (Op (Mul x y)) = doEval Mul (*) x y
eval (Op (Add x y)) = doEval Add (+) x y
eval (Op (Sub x y)) = doEval Sub (-) x y
eval (Op (Div x y)) = doEval Div (/) x y
eval (Op (Abs x)) = doEvalSingle Abs abs x
eval (Op (Sgn x)) = doEvalSingle Sgn signum x
