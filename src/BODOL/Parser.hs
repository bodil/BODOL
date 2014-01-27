module BODOL.Parser where

import BODOL.Types
import Text.ParserCombinators.Parsec hiding (string)

--- such dsl, wow

such :: a -> a
such = id

very :: Monad m => a -> m a
very = return

--- Parser

symbolLead :: Parser Char
symbolLead = letter <|> oneOf "_$&/=+~:<>|§?!*-"

symbolChar :: Parser Char
symbolChar = alphaNum <|> oneOf "_$&/=+~:<>|§?!*-."

symbol :: Parser LSymbol
symbol = do {
  a <- such symbolLead;
  d <- many symbolChar;
  very $ LSymbol (a:d) } <?> "symbol"

string :: Parser LString
string = do {
  char '"';
  s <- many $ noneOf "\"" <|> (char '\\' >> anyChar);
  char '"';
  return $ LString s }

integer :: Parser String
integer = many1 digit <?> "integer"

decimal :: Parser String
decimal = do
  int <- integer
  char '.'
  frac <- integer
  return $ int ++ "." ++ frac

fraction :: Parser String
fraction = do
  num <- integer
  char '/'
  denom <- integer
  return $ num ++ "/" ++ denom

number :: Parser LNumber
number = do {
  prefix <- optionMaybe $ char '-';
  num <- try decimal <|> try fraction <|> integer;
  case prefix of
       Nothing -> return $ LNumber num
       Just neg -> return $ LNumber (neg:num) }
