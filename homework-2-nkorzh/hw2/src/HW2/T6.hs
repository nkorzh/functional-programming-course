{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( ParseError (..),
    Parser (..),
    pChar,
    pEof,
    parseError,
    parseExpr,
    runP,
    pOk
  )
where

import Control.Applicative (Alternative (..), Applicative (..), many, some)
import Control.Monad (MonadPlus, mfilter, msum)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Scientific (scientific, toRealFloat)
import GHC.Natural (Natural)
import HW2.T1 (Annotated ((:#)), Except (Error, Success))
import HW2.T4 (Expr (Op, Val), Prim (Abs, Add, Div, Mul, Sgn, Sub))
import HW2.T5

data ParseError = ErrorAtPos Natural
  deriving (Show)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P exceptState) text = case runES exceptState (0, text) of
  Success (res :# _) -> Success res
  Error e -> Error e

-- | What happens when the string is empty?
-- runP returns Error (ErrorAtPos pos) if the string in empty or skips one char
-- | How does the parser state change when a character is consumed?
-- position is incremented, tail is given as the rest of the text
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    [] -> Error (ErrorAtPos pos)
    (c : cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  P (ES leftRunner) <|> P (ES rightRunner) = P $ ES \state ->
    let leftRunRes = leftRunner state
     in case leftRunRes of
          Success _ -> leftRunRes
          _ -> rightRunner state

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES \(pos, s) -> case s of
  [] -> Success (() :# (pos + 1, []))
  _ -> Error $ ErrorAtPos pos

pOk :: Parser ()
pOk = P $ ES \(pos, s) -> Success (() :# (pos, s))

-- | error position +1 here
pIsChar :: Char -> Parser String
pIsChar char = do
  parsedChar <- mfilter (== char) pChar
  return [parsedChar]

pString :: [Char] -> Parser String
pString string = msum (map pIsChar string)

pSpaces :: Parser String
pSpaces = do many (mfilter Data.Char.isSpace pChar)

pDigit :: Parser Integer
pDigit = P $ ES \(pos, s) -> case s of
  [] -> Error $ ErrorAtPos pos
  (c : cs) ->
    if (isDigit c)
      then Success (((toInteger . digitToInt) c) :# (pos + 1, cs))
      else Error $ ErrorAtPos pos

pDouble :: Parser Expr
pDouble = do
  _ <- pSpaces
  beforeDot <- some pDigit
  _ <- pIsChar '.'
  afterDot <- some pDigit
  _ <- pSpaces
  return
    ( let mantissa = beforeDot ++ afterDot
          expPower = negate $ length afterDot
          numSci = scientific (foldl1 (\x y -> (x * 10) + y) mantissa) expPower
          floatResult = toRealFloat numSci
       in Val floatResult
    )

-- | signature, operation
makeUnaryP :: String -> (Expr -> Prim Expr) -> Parser Expr
makeUnaryP signature opConstructor = do
  _ <- pSpaces
  _ <- pString signature
  _ <- pSpaces
  expr <- pUnary
  return $ Op (opConstructor expr)

makeBinaryP :: String -> (Expr -> Expr -> Prim Expr) -> Parser (Expr -> Expr -> Expr)
makeBinaryP signature opConstructor = do
  _ <- pSpaces
  _ <- pString signature
  _ <- pSpaces
  return \left right -> Op (opConstructor left right)

applyFirstToOthers :: Expr -> [Expr -> Expr] -> Expr
applyFirstToOthers = foldl (\leftArg waitingLeftArg -> waitingLeftArg leftArg)

pMulDiv :: Parser Expr
pMulDiv =
  let pMul = makeBinaryP "*" Mul
      pDiv = makeBinaryP "/" Div
   in do
        first <- pUnary
        others <-
          many
            ( do
                constructor <- pMul <|> pDiv
                unaryRightExpr <- pUnary
                return $ flip constructor unaryRightExpr
            )
        return $ applyFirstToOthers first others

pAddSub :: Parser Expr
pAddSub =
  let pAdd = makeBinaryP "+" Add
      pSub = makeBinaryP "-" Sub
   in do
        first <- pMulDiv
        others <-
          many
            ( do
                constructor <- pAdd <|> pSub
                mulDivRightExpr <- pMulDiv
                return $ flip constructor mulDivRightExpr
            )
        return $ applyFirstToOthers first others

pUnary :: Parser Expr
pUnary = pDouble <|> pAbs <|> pSgn <|> pExprInBraces
  where
    pAbs = makeUnaryP "abs" Abs
    pSgn = makeUnaryP "signum" Sgn

pExprInBraces :: Parser Expr
pExprInBraces = do
  _ <- pSpaces
  _ <- pIsChar '('
  _ <- pSpaces
  expr <- pAddSub
  _ <- pSpaces
  _ <- pIsChar ')'
  _ <- pSpaces
  return expr

pExpr :: Parser Expr
pExpr = do
  res <- pAddSub
  pEof
  return res

parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr
