module HW2.T5
  ( EvaluationError (..),
    ExceptState (..),
    eval,
    joinExceptState,
    mapExceptState,
    modifyExceptState,
    wrapExceptState,
    throwExceptState,
  )
where

import Control.Monad (unless)
import qualified Control.Monad
import HW2.T1 (Annotated (..), Except (..), mapAnnotated, mapExcept)
import HW2.T4 (Expr (Op, Val), Prim (Abs, Add, Div, Mul, Sgn, Sub))

-- | error, state, result
data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES stateF) = ES $ \s -> mapExcept mapAF (stateF s)
  where
    mapAF = mapAnnotated f

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES outer) = ES $ \s ->
  case outer s of
    Success ((ES inner) :# a) -> inner a
    Error e -> Error e

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState s a) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState s a) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

doEval ::
  (Double -> Double -> Prim Double) -> -- operation signature
  (Double -> Double -> Double) -> -- result evaluating function
  Expr -> -- left expression
  Expr -> -- right expression
  ExceptState EvaluationError [Prim Double] Double
doEval opName f x y = do
  ex <- eval x
  ey <- eval y
  modifyExceptState ((++) [opName ex ey])
  return (ex `f` ey)

doEvalSingle :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
doEvalSingle opName f x = do
  ex <- eval x
  modifyExceptState ((++) [opName ex])
  return $ f ex

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = return x
eval (Op (Abs x)) = doEvalSingle Abs abs x
eval (Op (Sgn x)) = doEvalSingle Sgn signum x
eval (Op (Mul x y)) = doEval Mul (*) x y
eval (Op (Add x y)) = doEval Add (+) x y
eval (Op (Sub x y)) = doEval Sub (-) x y
eval (Op (Div x y)) = do
  ex <- eval x
  ey <- eval y
  modifyExceptState ((++) [Div ex ey])
  unless (ey /= 0) (throwExceptState DivideByZero)
  return (ex / ey)
