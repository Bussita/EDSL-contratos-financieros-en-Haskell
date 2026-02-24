{-# LANGUAGE ScopedTypeVariables #-}
module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.Time (Day ,fromGregorian)
import Text.Parsec.Char (digit, char)

import Types
import AST

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef {
    T.reservedNames = ["zero", "one", "give", "and", "or", "truncate", "then", "scale", "get", "anytime", "let"],
    T.reservedOpNames = ["+", "-", "*", "/", "=", ";"],
    T.caseSensitive = False
}

reserved   = T.reserved lexer
integer    = T.integer lexer
float      = T.float lexer
parens     = T.parens lexer
identifier = T.identifier lexer
reservedOp = T.reservedOp lexer
whiteSpace = T.whiteSpace lexer
semi       = T.semi lexer

-- Comandos
parserComm :: Parser Comm
parserComm = chainl1 parserSingleComm (semi >> return Seq)

parserSingleComm :: Parser Comm
parserSingleComm = try parserAssign <|> parserEvalComm

parserAssign :: Parser Comm
parserAssign = do
    reserved "let"
    name <- identifier
    reservedOp "="
    c <- parserContract
    return (Assign name c)

parserEvalComm :: Parser Comm
parserEvalComm = Run <$> parserContract

-- Contratos
{-
chainl es equivalente a
parserContract = do
    t <- parserTerm
    rest t
  where
    rest acc = (do reserved "or"
                   t <- parserTerm
                   rest (Or acc t))
              <|> return acc
-}
parserContract :: Parser Contract
parserContract = chainl1 parserTerm (reserved "or" >> return Or)

parserTerm :: Parser Contract
parserTerm = chainl1 parserFactor (reserved "and" >> return And)

parserFactor :: Parser Contract
parserFactor = chainl1 parserPrefix (reserved "then" >> return Then)

parserPrefix :: Parser Contract
parserPrefix = parserGive
              <|> parserGet
              <|> parserAnytime
              <|> parserScale
              <|> parserTruncate
              <|> parserAtom

parserGive :: Parser Contract
parserGive = reserved "give" >> (Give <$> parserPrefix)

parserGet :: Parser Contract
parserGet = reserved "get" >> (Get <$> parserPrefix)

parserScale :: Parser Contract
parserScale = do
    reserved "scale"
    o <- parserObservable
    c <- parserPrefix
    return (Scale o c)

parserAnytime :: Parser Contract
parserAnytime = reserved "anytime" >> (Anytime <$> parserPrefix)

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
        Just (curr :: Currency) -> return (One curr)
        Nothing -> fail ("Error: " ++ c ++ " no es un tipo de moneda válido.")

-- Fechas

parserTime :: Parser Date
parserTime = do
    y <- count 4 digit
    _ <- char '-'
    m <- count 2 digit
    _ <- char '-'
    d <- count 2 digit
    whiteSpace
    return (fromGregorian (read y) (read m) (read d))

-- Observables
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
parserObsAtom = parens parserObservable
            <|> try parserNeg
            <|> try parserKonst
            <|> parserExternal

parserNeg :: Parser (Obs Double)
parserNeg = do
    reservedOp "-"
    o <- parserObsAtom
    return (Neg o)

parserKonst :: Parser (Obs Double)
parserKonst = do
    n <- try float <|> (fromIntegral <$> integer)
    return (Konst n)

parserExternal :: Parser (Obs Double)
parserExternal = do
    s <- identifier
    return (External s)

