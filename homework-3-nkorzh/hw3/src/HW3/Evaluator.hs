{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator (eval) where

import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT (..), catchE, runExceptT, throwE)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S
import qualified Data.Text as T
import HW3.Base

type HiResult m a = ExceptT HiError m a

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval e = runExceptT (eval' e)

eval' :: Monad m => HiExpr -> HiResult m HiValue
eval' (HiExprValue value) = return value
eval' (HiExprApply fExpression args) = do
  hiValue <- eval' fExpression
  case hiValue of
    (HiValueFunction hiFun) -> evalFunc hiFun args
    (HiValueString str) -> evalSlice' str (T.length) (\i j s -> HiValueString $ substring i j s) args
    (HiValueList sq) -> evalSlice' sq (S.length) (\i j s -> HiValueList $ subseq i j s) args
    _ -> throwE HiErrorInvalidFunction

evalFunc :: Monad m => HiFun -> [HiExpr] -> HiResult m HiValue
evalFunc f args = case f of
  HiFunMul ->
    catchInvalidArg
      [ (evalBinaryNum (*) args),
        (mulRepeatable args getString HiValueString),
        (mulRepeatable args getSeq HiValueList)
      ]
  HiFunAdd ->
    catchInvalidArg
      [ (evalBinaryNum (+) args),
        (evalBinaryString T.append args),
        (addSeq args)
      ]
  HiFunDiv ->
    catchInvalidArg
      [ ( do
            (left, right) <- toBinaryValTuple getNumber args
            if right == 0
              then throwE HiErrorDivideByZero
              else return $ HiValueNumber (left / right)
        ),
        (evalBinaryString divTexts args)
      ]
  HiFunSub -> evalBinaryNum (-) args
  -- boolean
  HiFunEquals -> evalCompare (==) args
  HiFunNotEquals -> evalCompare (/=) args
  HiFunLessThan -> evalCompare (<) args
  HiFunGreaterThan -> evalCompare (>) args
  HiFunNotLessThan -> evalCompare (>=) args
  HiFunNotGreaterThan -> evalCompare (<=) args
  HiFunAnd -> evalBinaryBool args True -- evaluating right if left == True, else return !True
  HiFunOr -> evalBinaryBool args False -- evaluating right if left == False, else return !False
  HiFunNot -> do
    singleVal <- toSingleArg args
    onlyArg <- getBool singleVal
    return $ HiValueBool $ not onlyArg
  HiFunIf ->
    do
      checkArity 3 args
      hiValue <- eval' $ head args
      condition <- getBool hiValue
      eval' $
        args
          !! if condition
            then 1
            else 2
  -- string and list
  HiFunLength -> catchInvalidArg [(evalStringLen args), (evalSeq args (HiValueNumber . toRational . length))]
  HiFunReverse -> catchInvalidArg [(evalStringS T.reverse args), (evalSeq args (HiValueList . S.reverse))]
  HiFunToUpper -> evalStringS T.toUpper args
  HiFunToLower -> evalStringS T.toLower args
  HiFunTrim -> evalStringS T.strip args
  HiFunList -> do
    listValues <- evalArgs args
    return $ HiValueList (S.fromList listValues)
  HiFunRange -> do
    (first, second) <- toBinaryValTuple getNumber args
    let lastV =
          if denominator first == 1
            then fromIntegral (floor second :: Int)
            else second
        res = map HiValueNumber [first .. lastV]
    return $ HiValueList (S.fromList res)
  HiFunFold -> do
    (funVal, listVal) <- toBinaryTuple args
    hiFun <- getFunc funVal
    evalFold hiFun listVal

-- | Seq functions
addSeq :: Monad m => [HiExpr] -> HiResult m HiValue
addSeq args = do
  (left, right) <- toBinaryValTuple getSeq args
  return $ HiValueList (left S.>< right)

evalSeq :: Monad m => [HiExpr] -> (Seq HiValue -> a) -> HiResult m a
evalSeq args seqF = do
  arg <- toSingleArg args
  s <- getSeq arg
  return $ seqF s

evalFold :: Monad m => HiFun -> HiValue -> HiResult m HiValue
evalFold fun val = do
  s <- getSeq val
  case fun of
    HiFunAdd ->
      catchInvalidArg
        [ foldSeq1 s (+) getNumber HiValueNumber,
          foldSeq1 s (T.append) getString HiValueString
        ]
    HiFunDiv ->
      catchInvalidArg
        [ foldSeq1 s (/) getNumber HiValueNumber,
          foldSeq1 s divTexts getString HiValueString
        ]
    HiFunMul -> foldSeq1 s (*) getNumber HiValueNumber
    HiFunAnd -> foldSeq1 s (&&) getBool HiValueBool
    HiFunOr -> foldSeq1 s (||) getBool HiValueBool
    _ -> throwE HiErrorInvalidArgument

foldSeq1 :: Monad m => (Seq HiValue) -> (a -> a -> a) -> (HiValue -> HiResult m a) -> (a -> HiValue) -> HiResult m HiValue
foldSeq1 seq' f extract wrap = do
  if S.length seq' == 0
    then return HiValueNull
    else do
      vals <- unpackSeq seq' extract
      return $ wrap $ foldl1 f vals

foldSeq :: Monad m => (Seq HiValue) -> (a -> a -> a) -> (HiValue -> HiResult m a) -> (a -> HiValue) -> a -> HiResult m HiValue
foldSeq seq' f extract wrap startVal = do
  vals <- unpackSeq seq' extract
  return $ wrap $ foldl f startVal vals

unpackSeq :: Monad m => (Seq HiValue) -> (HiValue -> HiResult m a) -> HiResult m (Seq a)
unpackSeq (xs :|> x) extract = do
  rational <- extract x
  tailV <- unpackSeq xs extract
  return (tailV :|> rational)
unpackSeq Empty _ = return Empty

subseq :: Int -> Int -> Seq a -> Seq a
subseq start stop =
  S.take len . S.drop start
  where
    len = stop - start

evalSlice :: Monad m => T.Text -> [HiExpr] -> HiResult m HiValue
evalSlice str [singleExpr] = do
  val <- toSingleArg [singleExpr]
  index <- getInt val
  return $
    if index < 0 || index >= T.length str
      then HiValueNull
      else HiValueString $ substring index (index + 1) str
evalSlice str [start, stop] = do
  (valStart, valStop) <- toBinaryTuple [start, stop]
  iStart <- getIntegerOrDefault valStart 0
  iStop <- getIntegerOrDefault valStop (T.length str)
  let fromPython ind = if ind < 0 then ind + T.length str else ind
  return $ HiValueString $ substring (fromPython iStart) (fromPython iStop) str
evalSlice _ _ = throwE HiErrorArityMismatch

evalSlice' :: Monad m => a -> (a -> Int) -> (Int -> Int -> a -> HiValue) -> [HiExpr] -> HiResult m HiValue
evalSlice' sliceAble getLen getSlice [singleExpr] = do
  val <- toSingleArg [singleExpr]
  index <- getInt val
  return $
    if index < 0 || index >= getLen sliceAble
      then HiValueNull
      else getSlice index (index + 1) sliceAble
evalSlice' sliceAble getLen getSlice [start, stop] = do
  (valStart, valStop) <- toBinaryTuple [start, stop]
  iStart <- getIntegerOrDefault valStart 0
  iStop <- getIntegerOrDefault valStop (getLen sliceAble)
  let fromPython ind = if ind < 0 then ind + getLen sliceAble else ind
  return $ getSlice (fromPython iStart) (fromPython iStop) sliceAble
evalSlice' _ _ _ _ = throwE HiErrorArityMismatch

-- | String functions
substring :: Int -> Int -> T.Text -> T.Text
substring start stop =
  T.take len . T.drop start
  where
    len = stop - start

evalStringS :: Monad m => (T.Text -> T.Text) -> [HiExpr] -> HiResult m HiValue
evalStringS f args = evalString f args HiValueString

evalStringLen :: Monad m => [HiExpr] -> HiResult m HiValue
evalStringLen args = evalString (toRational . T.length) args HiValueNumber

evalString :: Monad m => (T.Text -> a) -> [HiExpr] -> (a -> HiValue) -> HiResult m HiValue
evalString f args valWrapper = do
  values <- toSingleArg args
  str <- getString values
  return $ valWrapper (f str)

evalBinaryString :: Monad m => (T.Text -> T.Text -> T.Text) -> [HiExpr] -> HiResult m HiValue
evalBinaryString f args = do
  (left, right) <- toBinaryValTuple getString args
  return $ HiValueString (left `f` right)

mulRepeatable :: (Monad m, Semigroup a) => [HiExpr] -> (HiValue -> HiResult m a) -> (a -> HiValue) -> HiResult m HiValue
mulRepeatable args extract wrap = do
  (lVal, rVal) <- toBinaryTuple args
  str <- extract lVal
  int <- getInt rVal
  if int <= 0
    then throwE HiErrorInvalidArgument
    else return $ wrap (stimes int str)

divTexts :: T.Text -> T.Text -> T.Text
divTexts s1 s2 = s1 `T.append` (T.pack "/") `T.append` s2

-- | numeric and boolean functions
evalBinaryNum :: Monad m => (Rational -> Rational -> Rational) -> [HiExpr] -> HiResult m HiValue
evalBinaryNum f args = do
  (left, right) <- toBinaryValTuple getNumber args
  return $ HiValueNumber (left `f` right)

evalBool :: Monad m => HiExpr -> HiResult m Bool
evalBool hiExpr = do
  evaluated <- eval' hiExpr
  getBool evaluated

evalBinaryBool :: Monad m => [HiExpr] -> Bool -> HiResult m HiValue
evalBinaryBool args leftValToEvalRight = do
  checkArity 2 args
  left <- evalBool $ head args
  if leftValToEvalRight == left
    then do
      right <- evalBool $ args !! 1
      return $ HiValueBool (right)
    else return $ HiValueBool (not leftValToEvalRight)

-- | compare
evalCompare :: Monad m => (HiValue -> HiValue -> Bool) -> [HiExpr] -> HiResult m HiValue
evalCompare f args = do
  (left, right) <- toBinaryTuple args
  return $ HiValueBool (left `f` right)

-- | arglist transformation
toBinaryValTuple :: Monad m => (HiValue -> HiResult m a) -> [HiExpr] -> HiResult m (a, a)
toBinaryValTuple extract args = do
  (lVal, rVal) <- toBinaryTuple args
  left <- extract lVal
  right <- extract rVal
  return (left, right)

toBinaryTuple :: Monad m => [HiExpr] -> HiResult m (HiValue, HiValue)
toBinaryTuple args = do
  values <- checkArityAndEval 2 args
  left <- head values
  right <- values !! 1
  return (left, right)

toSingleArg :: Monad m => [HiExpr] -> HiResult m HiValue
toSingleArg args = do
  values <- checkArityAndEval 1 args
  head values

evalArgs :: Monad m => [HiExpr] -> HiResult m [HiValue]
evalArgs [] = return []
evalArgs (x : xs) = do
  val <- eval' x
  vTail <- evalArgs xs
  return $ val : vTail

checkArityAndEval :: Monad m => Int -> [HiExpr] -> HiResult m [HiResult m HiValue]
checkArityAndEval arity args = do
  checkArity arity args
  return $ map eval' args

checkArity :: Monad m => Int -> [a] -> HiResult m ()
checkArity arity list = when (length list /= arity) $ throwE HiErrorArityMismatch

-- | getters
getFunc :: Monad m => HiValue -> HiResult m HiFun
getFunc (HiValueFunction f) = return f
getFunc _ = throwE HiErrorInvalidArgument

getString :: Monad m => HiValue -> HiResult m T.Text
getString (HiValueString str) = return str
getString _ = throwE HiErrorInvalidArgument

getNumber :: Monad m => HiValue -> HiResult m Rational
getNumber (HiValueNumber num) = return num
getNumber _ = throwE HiErrorInvalidArgument

getSeq :: Monad m => HiValue -> HiResult m (Seq HiValue)
getSeq (HiValueList s) = return s
getSeq _ = throwE HiErrorInvalidArgument

getInt :: Monad m => HiValue -> HiResult m Int
getInt value = do
  rational <- getNumber value
  if denominator rational == 1
    then return $ fromIntegral (numerator rational)
    else throwE HiErrorInvalidArgument

getIntegerOrDefault :: Monad m => HiValue -> Int -> HiResult m Int
getIntegerOrDefault HiValueNull defaultInt = return defaultInt
getIntegerOrDefault otherValue _ = getInt otherValue

getBool :: Monad m => HiValue -> HiResult m Bool
getBool (HiValueBool bool) = return bool
getBool _ = throwE HiErrorInvalidArgument

catchInvalidArg :: Monad m => [HiResult m HiValue] -> HiResult m HiValue
catchInvalidArg (x : xs) =
  catchE
    x
    ( \case
        HiErrorInvalidArgument -> catchInvalidArg xs
        cons -> throwE cons
    )
catchInvalidArg [] = throwE HiErrorInvalidArgument
