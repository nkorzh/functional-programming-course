module HW3.ParserBase where

import Data.Map (Map, fromList, lookup)
import Data.Void (Void)
import HW3.Base
import Text.Megaparsec (between, choice, try)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

mapIdentifiers :: Map String HiFun
mapIdentifiers = fromList listIdentifiers

-- | Use only when sure you've found correct token from mapIdentifiers
funIdentifier :: String -> HiFun
funIdentifier name = case Data.Map.lookup name mapIdentifiers of
  Just hiFun -> hiFun
  _ -> undefined

type Parser = MP.Parsec Void String

skipSpace :: Parser ()
skipSpace = space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: String -> Parser String
symbol = L.symbol skipSpace

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

funcToken :: Parser String
funcToken = choice $ map (\(name, _) -> try . string $ name) listIdentifiers
