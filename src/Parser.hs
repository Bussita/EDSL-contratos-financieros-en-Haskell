{-# LANGUAGE ScopedTypeVariables #-}
module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.Time (Day, fromGregorian)
import Text.Parsec.Char (digit, char)

import Types
import AST

-- Lexer

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef
  { T.reservedNames   = [ "zero", "one", "give", "and", "or"
                        , "truncate", "then", "scale", "get", "anytime"
                        , "let"
                          -- comandos billetera
                        , "deposit", "propose", "sign", "execute"
                          -- observable
                        , "balance"
                        ]
  , T.reservedOpNames = ["+", "-", "*", "/", "=", ";"]
  , T.caseSensitive   = False
  }

reserved   = T.reserved   lexer
integer    = T.integer    lexer
float      = T.float      lexer
parens     = T.parens     lexer
identifier = T.identifier lexer
reservedOp = T.reservedOp lexer
whiteSpace = T.whiteSpace lexer
semi       = T.semi       lexer

-- Comandos
-- Punto de entrada: uno o más comandos separados por ';'.
parserComm :: Parser Comm
parserComm = chainl1 parserSingleComm (semi >> return Seq)

parserSingleComm :: Parser Comm
parserSingleComm =
      try parserAssign
  <|> try parserDeposit
  <|> try parserPropose
  <|> try parserSign
  <|> try parserExecute
  <|> parserEvalComm

parserAssign :: Parser Comm
parserAssign = do
  reserved "let"
  name <- identifier
  reservedOp "="
  c <- parserContract
  return (Assign name c)

-- deposit <parte> <monto> <moneda>
parserDeposit :: Parser Comm
parserDeposit = do
  reserved "deposit"
  party <- identifier
  amt   <- parserNumber
  cur   <- parserCurrency
  return (Deposit party cur amt)

-- propose <nombre> <contrato>
parserPropose :: Parser Comm
parserPropose = do
  reserved "propose"
  name <- identifier
  c    <- parserContract
  return (Propose name c)

-- sign <nombre> <parte>
parserSign :: Parser Comm
parserSign = do
  reserved "sign"
  name  <- identifier
  party <- identifier
  return (Sign name party)

-- execute <nombre>
parserExecute :: Parser Comm
parserExecute = do
  reserved "execute"
  name <- identifier
  return (Execute name)

parserEvalComm :: Parser Comm
parserEvalComm = Run <$> parserContract

-- Contratos

parserContract :: Parser Contract
parserContract = chainl1 parserTerm (reserved "or" >> return Or)

parserTerm :: Parser Contract
parserTerm = chainl1 parserFactor (reserved "and" >> return And)

parserFactor :: Parser Contract
parserFactor = chainl1 parserPrefix (reserved "then" >> return Then)

parserPrefix :: Parser Contract
parserPrefix =
      parserGive
  <|> parserGet
  <|> parserAnytime
  <|> parserScale
  <|> parserTruncate
  <|> parserAtom

parserGive     = reserved "give"     >> (Give    <$> parserPrefix)
parserGet      = reserved "get"      >> (Get     <$> parserPrefix)
parserAnytime  = reserved "anytime"  >> (Anytime <$> parserPrefix)

parserScale :: Parser Contract
parserScale = do
  reserved "scale"
  o <- parserObservable
  c <- parserPrefix
  return (Scale o c)

parserTruncate :: Parser Contract
parserTruncate = do
  reserved "truncate"
  t <- parserTime
  c <- parserPrefix
  return (Truncate t c)

parserAtom :: Parser Contract
parserAtom = parens parserContract <|> pZero <|> parserOne <|> parserVar

pZero :: Parser Contract
pZero = reserved "zero" >> return Zero

parserVar :: Parser Contract
parserVar = Var <$> identifier

parserOne :: Parser Contract
parserOne = do
  reserved "one"
  c <- identifier
  case readMaybe (map toUpper c) of
    Just (cur :: Currency) -> return (One cur)
    Nothing -> fail ("'" ++ c ++ "' no es una moneda válida (USD, EUR, ARS, GBP).")

-- ─── Fechas ───────────────────────────────────────────────────────────────────

parserTime :: Parser Date
parserTime = do
  y <- count 4 digit
  _ <- char '-'
  m <- count 2 digit
  _ <- char '-'
  d <- count 2 digit
  whiteSpace
  return (fromGregorian (read y) (read m) (read d))

-- ─── Observables ─────────────────────────────────────────────────────────────

parserObservable :: Parser (Obs Double)
parserObservable = chainl1 parserMultDiv op
  where
    op = (reservedOp "+" >> return Add)
     <|> (reservedOp "-" >> return Sub)

parserMultDiv :: Parser (Obs Double)
parserMultDiv = chainl1 parserObsAtom op
  where
    op = (reservedOp "*" >> return Mul)
     <|> (reservedOp "/" >> return Div)

parserObsAtom :: Parser (Obs Double)
parserObsAtom =
      parens parserObservable
  <|> try parserNeg
  <|> try parserKonst
  <|> try parserBalance   -- antes de parserExternal para evitar consumir "balance"
  <|> parserExternal

-- Observable: -<obs>
parserNeg :: Parser (Obs Double)
parserNeg = do
  reservedOp "-"
  o <- parserObsAtom
  return (Neg o)

-- Observable: número literal
parserKonst :: Parser (Obs Double)
parserKonst = Konst <$> parserNumber

-- Observable: balance <parte> <moneda>
-- Permite cosas como:  scale (balance Alice USD) one USD
parserBalance :: Parser (Obs Double)
parserBalance = do
  reserved "balance"
  party <- identifier
  cur   <- parserCurrency
  return (Balance party cur)

-- Observable: nombre externo (cotización del oráculo)
parserExternal :: Parser (Obs Double)
parserExternal = External <$> identifier

-- Parsers auxiliares

-- Parsea un número como Double (entero o flotante).
parserNumber :: Parser Double
parserNumber = try float <|> (fromIntegral <$> integer)

-- Parsea un identificador de moneda (USD, EUR, ARS, GBP).
parserCurrency :: Parser Currency
parserCurrency = do
  s <- identifier
  case readMaybe (map toUpper s) of
    Just (cur :: Currency) -> return cur
    Nothing -> fail ("'" ++ s ++ "' no es una moneda válida (USD, EUR, ARS, GBP).")