module HW3.Base
  ( HiError (..),
    HiExpr (..),
    HiFun (..),
    HiMonad (..),
    HiValue (..),
    listIdentifiers,
  )
where

import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Text (Text)

data HiFun
  = HiFunDiv --p7 l
  | HiFunMul --p7 l
  | HiFunAdd --p6 l
  | HiFunSub --p6 l
  | HiFunNot
  | HiFunAnd --p3 r
  | HiFunOr --p2 r
  | HiFunLessThan --p4
  | HiFunGreaterThan --p4
  | HiFunEquals --p4
  | HiFunNotLessThan --p4
  | HiFunNotGreaterThan --p4
  | HiFunNotEquals --p4
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  deriving (Show, Eq, Ord)

data HiValue
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  deriving (Show, Eq, Ord)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Show, Eq, Ord)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)

data HiAction
  = HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

--instance Eq HiValue where
--  (==) (HiValueBool a) (HiValueBool b) = (==) a b
--  (==) (HiValueNumber a) (HiValueNumber b) = (==) a b
--  (==) (HiValueFunction a) (HiValueFunction b) = (==) a b
--  (==) (HiValueString a) (HiValueString b) = (==) a b
--  (==) HiValueNull HiValueNull = True
--  (==) _ _ = False
--
--instance Ord HiValue where
--  compare (HiValueString a) (HiValueString b) = compare a b
--  compare (HiValueBool a) (HiValueBool b) = compare a b
--  compare (HiValueNumber a) (HiValueNumber b) = compare a b
--  compare (HiValueFunction a) (HiValueFunction b) = compare a b
--  compare (HiValueBool _) (HiValueNumber _) = LT
--  compare (HiValueNumber _) (HiValueBool _) = GT
--  compare (HiValueNumber _) (HiValueFunction _) = LT
--  compare (HiValueFunction _) (HiValueNumber _) = GT
--  compare (HiValueBool _) (HiValueFunction _) = LT
--  compare (HiValueFunction _) (HiValueBool _) = GT
--  compare (HiValueString _) (HiValueNull) = GT
--  compare (HiValueNull) (HiValueString _) = LT
--  compare _ _ = undefined

listIdentifiers :: [(String, HiFun)]
listIdentifiers =
  [ ("list", HiFunList),
    ("range", HiFunRange),
    ("fold", HiFunFold),
    ("length", HiFunLength),
    ("to-upper", HiFunToUpper),
    ("to-lower", HiFunToLower),
    ("reverse", HiFunReverse),
    ("trim", HiFunTrim),
    ("not-less-than", HiFunNotLessThan),
    ("not-greater-than", HiFunNotGreaterThan),
    ("not-equals", HiFunNotEquals),
    ("less-than", HiFunLessThan),
    ("greater-than", HiFunGreaterThan),
    ("equals", HiFunEquals),
    ("if", HiFunIf),
    ("div", HiFunDiv),
    ("mul", HiFunMul),
    ("add", HiFunAdd),
    ("sub", HiFunSub),
    ("not", HiFunNot),
    ("and", HiFunAnd),
    ("or", HiFunOr)
  ]
