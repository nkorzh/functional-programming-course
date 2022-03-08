module HW3.Parser (parse) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Void (Void)
import HW3.Base
import HW3.ParserBase
import Text.Megaparsec (choice, eof, many, manyTill, notFollowedBy, optional, try, (<?>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parse :: String -> Either (MP.ParseErrorBundle String Void) HiExpr
parse = MP.runParser hiExpr "some.txt"

hiExpr :: Parser HiExpr
hiExpr = skipSpace >> (expr <* eof)

enumerateExpr :: Parser [HiExpr]
enumerateExpr = lexeme $
  inParens $ do
    first <- expr
    others <-
      many
        ( do
            _ <- lexeme $ char ','
            lexeme $ expr
        )
    return $ first : others

application :: Parser HiExpr
application = do
  firstExpr <- hiExprValue -- token + application
  others <- many enumerateExpr -- [[HiExpr]]
  return $ makeHighOrderFunc firstExpr others

makeHighOrderFunc :: HiExpr -> [[HiExpr]] -> HiExpr
makeHighOrderFunc = foldl HiExprApply

makeBinary :: HiFun -> HiExpr -> HiExpr -> HiExpr
makeBinary fun left right = HiExprApply (HiExprValue $ HiValueFunction fun) [left, right]

binaryL, binaryR, binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryL name f = InfixL $ makeBinary f <$ symbol name
binaryR name f = InfixR $ makeBinary f <$ symbol name
binaryN name f = InfixN $ makeBinary f <$ symbol name

binaryLNotFollowed :: String -> String -> HiFun -> Operator Parser HiExpr
binaryLNotFollowed name nfSymbol f = InfixL $ makeBinary f <$ try (symbol name <* notFollowedBy (symbol nfSymbol))

table :: [[Operator Parser HiExpr]]
table =
  [ [ binaryL "*" HiFunMul,
      binaryLNotFollowed "/" "=" HiFunDiv
    ],
    [ binaryL "+" HiFunAdd,
      binaryL "-" HiFunSub
    ],
    [ binaryN "==" HiFunEquals,
      binaryN "/=" HiFunNotEquals,
      binaryN "<=" HiFunNotGreaterThan,
      binaryN ">=" HiFunNotLessThan,
      binaryN ">" HiFunGreaterThan,
      binaryN "<" HiFunLessThan
    ],
    [ binaryR "&&" HiFunAnd
    ],
    [ binaryR "||" HiFunOr
    ]
  ]

exprTerm :: Parser HiExpr
exprTerm = try application <|> inParens expr

expr :: Parser HiExpr
expr = makeExprParser exprTerm table <?> "expression"

hiExprValue :: Parser HiExpr
hiExprValue = exprValue <|> inParens hiExprValue

exprValue :: Parser HiExpr
exprValue =
  choice
    [ valueBool,
      valueFunc,
      valueNumber,
      valueString,
      valueList,
      valueNull
    ]

valueNull :: Parser HiExpr
valueNull = lexeme $ do
  _ <- string "null"
  return $ HiExprValue $ HiValueNull

valueString :: Parser HiExpr
valueString = lexeme $ do
  str <- char '\"' >> manyTill L.charLiteral (char '\"')
  return $ HiExprValue $ HiValueString (pack str)

valueNumber :: Parser HiExpr
valueNumber = lexeme $ do
  value <- L.signed skipSpace L.scientific
  return $ HiExprValue $ HiValueNumber $ toRational value

valueBool :: Parser HiExpr
valueBool = lexeme $ do
  bool <- False <$ string "false" <|> True <$ string "true"
  return $ HiExprValue $ HiValueBool bool

valueList :: Parser HiExpr
valueList = lexeme $ do
  _ <- symbol "["
  listVals <- optional $ do
    first <- expr
    others <-
      many
        ( do
            _ <- symbol ","
            lexeme $ expr
        )
    return (first : others)
  _ <- symbol "]"
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) (fromMaybe [] listVals)

valueFunc :: Parser HiExpr
valueFunc = lexeme $ do
  tok <- funcToken
  return $ HiExprValue $ HiValueFunction (funIdentifier tok)
