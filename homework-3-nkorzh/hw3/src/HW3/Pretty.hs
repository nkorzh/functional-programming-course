module HW3.Pretty (prettyValue) where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Map (fromList, lookup)
import Data.Ratio (denominator, numerator)
import Data.Sequence.Internal (Seq)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Internal (Doc, pretty)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import HW3.Base
import Numeric (showFFloat)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue value = case value of
  (HiValueBool bool) -> pretty . T.toLower . T.pack . show $ bool
  (HiValueFunction func) -> pretty . funcToIdentifier $ func
  (HiValueNumber number) -> prettyRational number
  (HiValueString str) -> pretty $ "\"" ++ (T.unpack str) ++ "\""
  (HiValueList s) -> prettySeq s
  HiValueNull -> pretty "null"
  smthElse -> error $ "prettyValue undefined for " ++ show smthElse

-- | All functions should be enumerated in that list
prettySeq :: Seq HiValue -> Doc AnsiStyle
prettySeq s =
  pretty $
    if null s
      then "[ ]"
      else "[ " ++ intercalate (", ") (map (show . prettyValue) (toList s)) ++ " ]"

funcToIdentifier :: HiFun -> T.Text
funcToIdentifier func = case Data.Map.lookup func mapFunctions of
  Just identifier -> T.pack identifier
  _ -> undefined
  where
    mapFunctions = fromList $ map (\(a, b) -> (b, a)) listIdentifiers

prettyRational :: Rational -> Doc ann
prettyRational number
  | printAsInteger number = pretty (numerator number)
  | printAsDouble number = pretty $ showFFloat Nothing (fromRational number :: Double) ""
  | otherwise = pretty res
  where
    denom = denominator number
    (integer, _) = properFraction number
    fract = number - toRational integer
    num = numerator $ abs fract
    sign = if fract < 0 then "-" else "+"
    showFract = show num ++ "/" ++ show denom
    res =
      if integer == 0
        then
          if fract > 0
            then showFract
            else sign ++ showFract
        else show integer ++ " " ++ sign ++ " " ++ showFract

printAsDouble :: Rational -> Bool
printAsDouble num = denomWithout2 == 1
  where
    denom = denominator num
    denomWithout5 = removeBase denom 5
    denomWithout2 = removeBase denomWithout5 2

printAsInteger :: Rational -> Bool
printAsInteger = (== 1) . denominator

removeBase :: Integer -> Integer -> Integer
removeBase number base
  | number `mod` base /= 0 = number
  | otherwise = removeBase (div number theirGcd) base
  where
    theirGcd = gcd base number
