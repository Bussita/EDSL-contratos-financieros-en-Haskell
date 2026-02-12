import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

import AST

-- Hacemos un lexer con las palabras que queremos
lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef {
    T.reservedNames = ["zero", "one", "give", "and", "or", "truncate", "scale", "get", "anytime"],
    T.caseSensitive = False -- con esto acepta palabras reservadas como Zero o zero.
}

-- Queremos usar las funciones estándar de parsec pero con la extensión Token
-- entonces les damos alias así no hay que escribir T. cada vez que las usamos
reserved   = T.reserved lexer
integer    = T.integer lexer
float      = T.float lexer
parens     = T.parens lexer
identifier = T.identifier lexer
reservedOp = T.reservedOp lexer
whiteSpace = T.whiteSpace lexer

contractParser :: Parser Contract
-- Build expression parser construye un parser de expresiones para term
-- con operadores de table 
contractParser = E.buildExpressionParser table term

term :: Parser Contract
term = parens contractParser <|> parseZero <|> parseOne <|> parseGive <|> parseScale <|> parseGet <|> parseAnytime

parseZero :: Parser Contract
parseZero = reserved "zero" >>= (\_ -> return Zero)

parseOne :: Parser Contract
parseOne = do
              reserved "one"
              c <- parseCurrency
              return (One c)


parseCurrency = do
                  parse

