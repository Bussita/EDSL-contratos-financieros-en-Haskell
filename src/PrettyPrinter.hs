module PrettyPrinter where

import Data.Time (Day)
import Data.List (intercalate)

import AST
import Types

{- Si mostramos en nuestro pretty printer con precedencias correctas,
muchas veces ahorramos parentesis. Esto nos es util por ejemplo con
contratos y observables, que nuestra gramática divide los operadores en
varios niveles de precedencia.

Contratos, recordando que sus precedencias son
--     0 - or
--     1 - and
--     2 - then
--     3 - prefijos (give, get, scale, truncate, anytime)
--     4 - átomos (zero, one, var, paréntesis)
-}
ppContract :: Contract -> String
ppContract = ppContractPrec 0

ppContractPrec :: Int -> Contract -> String
ppContractPrec _ Zero      = "zero"
ppContractPrec _ (One c)   = "one " ++ ppCurrency c
ppContractPrec _ (Var x)   = x

ppContractPrec p (Or c1 c2) =
    parensIf (p > 0) $ ppMaybeParens 0 c1 ++ " or " ++ ppMaybeParens 0 c2

ppContractPrec p (And c1 c2) =
    parensIf (p > 1) $ ppMaybeParens 1 c1 ++ " and " ++ ppMaybeParens 1 c2

ppContractPrec p (Then c1 c2) =
    parensIf (p > 2) $ ppMaybeParens 2 c1 ++ " then " ++ ppMaybeParens 2 c2

ppContractPrec p (Give c) =
    parensIf (p > 3) $ "give " ++ ppContractPrec 3 c

ppContractPrec p (Get c) =
    parensIf (p > 3) $ "get " ++ ppContractPrec 3 c

ppContractPrec p (Anytime c) =
    parensIf (p > 3) $ "anytime " ++ ppContractPrec 3 c

ppContractPrec p (Scale o c) =
    parensIf (p > 3) $ "scale " ++ ppObs o ++ " " ++ ppContractPrec 3 c

ppContractPrec p (Truncate d c) =
    parensIf (p > 3) $ "truncate " ++ ppDate d ++ " " ++ ppContractPrec 3 c

-- Paréntesis de claridad: si un hijo de un operador infijo es otro
-- operador infijo distinto, se agregan paréntesis aunque no sean
-- estrictamente necesarios por precedencia.
ppMaybeParens :: Int -> Contract -> String
ppMaybeParens prec child
    | isInfix child && infixPrec child /= prec = "(" ++ ppContractPrec 0 child ++ ")"
    | otherwise = ppContractPrec (prec + 1) child

isInfix :: Contract -> Bool
isInfix (Or _ _)   = True
isInfix (And _ _)  = True
isInfix (Then _ _) = True
isInfix _          = False

infixPrec :: Contract -> Int
infixPrec (Or _ _)   = 0
infixPrec (And _ _)  = 1
infixPrec (Then _ _) = 2
infixPrec _          = 4

-- Observables, recordando que la precedencia es:
--     0 - suma, resta
--     1 - multiplicación, división
--     2 - átomos (constantes, externos, paréntesis)

ppObs :: Show a => Obs a -> String
ppObs = ppObsPrec 0

ppObsPrec :: Show a => Int -> Obs a -> String
ppObsPrec _ (Konst x)    = showNum x
ppObsPrec _ (External s) = s

ppObsPrec p (Add a b) =
    parensIf (p > 0) $ ppObsPrec 0 a ++ " + " ++ ppObsPrec 1 b

ppObsPrec p (Sub a b) =
    parensIf (p > 0) $ ppObsPrec 0 a ++ " - " ++ ppObsPrec 1 b

ppObsPrec p (Mul a b) =
    parensIf (p > 1) $ ppObsPrec 1 a ++ " * " ++ ppObsPrec 2 b

ppObsPrec p (Div a b) =
    parensIf (p > 1) $ ppObsPrec 1 a ++ " / " ++ ppObsPrec 2 b

ppObsPrec p (Neg a) =
    parensIf (p > 1) $ "-" ++ ppObsPrec 2 a

-- Comandos 
ppComm :: Comm -> String
ppComm (Assign name c) = "let " ++ name ++ " = " ++ ppContract c
ppComm (Run c)         = ppContract c
ppComm (Seq c1 c2)     = ppComm c1 ++ ";\n" ++ ppComm c2

-- Cashflows

ppCashflow :: Cashflow -> String
ppCashflow cf =
    ppDate (fecha cf)
    ++ "  " ++ ppAmount (cantidad cf)
    ++ "  " ++ desde cf ++ " → " ++ hacia cf

ppAmount :: Amount -> String
ppAmount (Amount v c) = showDouble v ++ " " ++ ppCurrency c

ppCashflows :: [Cashflow] -> String
ppCashflows []  = "(sin flujos de caja)"
ppCashflows cfs =
    let header = "Fecha        Monto          Desde → Hacia"
        sep    = replicate (length header) '-'
        rows   = map ppCashflow cfs
    in  unlines (header : sep : rows)

-- Errores
ppError :: EvalError -> String
ppError DivByZero        = "ERROR (DivByZero): división por cero"
ppError (UnknownObs s)   = "ERROR (UnknownObs): observable desconocido '" ++ s ++ "'"
ppError (EvalMsg s)      = "ERROR (EvalMsg): " ++ s


ppCurrency :: Currency -> String
ppCurrency USD = "USD"
ppCurrency EUR = "EUR"
ppCurrency ARS = "ARS"
ppCurrency GBP = "GBP"

ppDate :: Day -> String
ppDate = show  -- day ya es YYYY-MM-DD

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

-- Mostrar un número sin el constructor Show (evita quotes, etc.)
showNum :: Show a => a -> String
showNum = show

-- Mostrar un double sin decimales cuando es entero
showDouble :: Double -> String
showDouble x
    | x == fromIntegral (round x) = show (round x :: Integer)
    | otherwise                   = show x
