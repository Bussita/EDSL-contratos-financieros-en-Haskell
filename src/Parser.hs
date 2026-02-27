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

-- ─── Lexer ────────────────────────────────────────────────────────────────────

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef
  { T.reservedNames   = [ "zero", "one", "give", "and", "or"
                        , "truncate", "then", "scale"
                        , "let"
                        , "deposit", "propose", "sign", "execute"
                        , "balance"
                        , "if", "else"   -- condicionales
                        , "setfecha"      -- cambio de fecha
                        ]
  , T.reservedOpNames = ["+", "-", "*", "/", "=", ";", ">", "<", ">=", "<=", "=="]
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

-- ─── Comandos ─────────────────────────────────────────────────────────────────

parserComm :: Parser Comm
parserComm = chainl1 parserSingleComm (semi >> return Seq)

parserSingleComm :: Parser Comm
parserSingleComm =
      try parserAssign
  <|> try parserDeposit
  <|> try parserPropose
  <|> try parserSign
  <|> try parserExecute
  <|> try parserSetFecha
  <|> parserEvalComm

parserAssign :: Parser Comm
parserAssign = do
  reserved "let"
  name <- identifier
  reservedOp "="
  c <- parserContract
  return (Assign name c)

parserDeposit :: Parser Comm
parserDeposit = do
  reserved "deposit"
  party <- identifier
  amt   <- parserNumber
  cur   <- parserCurrency
  return (Deposit party cur amt)

parserPropose :: Parser Comm
parserPropose = do
  reserved "propose"
  name <- identifier
  c    <- parserContract
  return (Propose name c)

parserSign :: Parser Comm
parserSign = do
  reserved "sign"
  name  <- identifier
  party <- identifier
  return (Sign name party)

parserExecute :: Parser Comm
parserExecute = do
  reserved "execute"
  name <- identifier
  return (Execute name)

-- setfecha YYYY-MM-DD
parserSetFecha :: Parser Comm
parserSetFecha = do
  reserved "setfecha"
  d <- parserTime
  return (SetFecha d)

parserEvalComm :: Parser Comm
parserEvalComm = Run <$> parserContract

-- ─── Contratos ────────────────────────────────────────────────────────────────

parserContract :: Parser Contract
parserContract = chainl1 parserTerm (reserved "or" >> return Or)

parserTerm :: Parser Contract
parserTerm = chainl1 parserFactor (reserved "and" >> return And)

parserFactor :: Parser Contract
parserFactor = chainl1 parserPrefix (reserved "then" >> return Then)

parserPrefix :: Parser Contract
parserPrefix =
      parserGive
  <|> parserScale
  <|> parserTruncate
  <|> try parserIf      -- try porque empieza con keyword "if"
  <|> parserAtom

parserGive :: Parser Contract
parserGive = reserved "give" >> (Give <$> parserPrefix)

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

-- | if <cond> then <contrato> else <contrato>
parserIf :: Parser Contract
parserIf = do
  reserved "if"
  cond <- parserObsBool
  reserved "then"
  c1 <- parserPrefix
  reserved "else"
  c2 <- parserPrefix
  return (If cond c1 c2)

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

-- ─── Observables booleanos ────────────────────────────────────────────────────
-- Gramática: <obs> <op> <obs>
-- donde <op> es >, <, >=, <=, ==

parserObsBool :: Parser ObsBool
parserObsBool = do
  left <- parserObservable
  op   <- parserBoolOp
  right <- parserObservable
  return (op left right)

parserBoolOp :: Parser (Obs Double -> Obs Double -> ObsBool)
parserBoolOp =
      (try (reservedOp ">=") >> return Gte)
  <|> (try (reservedOp "<=") >> return Lte)
  <|> (try (reservedOp "==") >> return Eq)
  <|> (reservedOp ">"        >> return Gt)
  <|> (reservedOp "<"        >> return Lt)

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

-- ─── Observables numéricos ────────────────────────────────────────────────────

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
  <|> try parserBalance
  <|> parserExternal

parserNeg :: Parser (Obs Double)
parserNeg = do
  reservedOp "-"
  o <- parserObsAtom
  return (Neg o)

parserKonst :: Parser (Obs Double)
parserKonst = Konst <$> parserNumber

parserBalance :: Parser (Obs Double)
parserBalance = do
  reserved "balance"
  party <- identifier
  cur   <- parserCurrency
  return (Balance party cur)

parserExternal :: Parser (Obs Double)
parserExternal = External <$> identifier

-- Parsers Auxiliares

parserNumber :: Parser Double
parserNumber = try float <|> (fromIntegral <$> integer)

parserCurrency :: Parser Currency
parserCurrency = do
  s <- identifier
  case readMaybe (map toUpper s) of
    Just (cur :: Currency) -> return cur
    Nothing -> fail ("'" ++ s ++ "' no es una moneda válida (USD, EUR, ARS, GBP).")