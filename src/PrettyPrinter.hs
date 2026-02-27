module PrettyPrinter where

import Data.Time (Day)
import qualified Data.Map as Map
import Data.Map (Map)

import AST
import Types

-- ─── Contratos ────────────────────────────────────────────────────────────────

ppContract :: Contract -> String
ppContract = ppContractPrec 0

ppContractPrec :: Int -> Contract -> String
ppContractPrec _ Zero      = "zero"
ppContractPrec _ (One c)   = "one " ++ ppCurrency c
ppContractPrec _ (Var x)   = x -- El PP no expande los alias de contrato

ppContractPrec p (Or c1 c2) =
  parensIf (p > 0) $ ppMaybeParens 0 c1 ++ " or " ++ ppMaybeParens 0 c2

ppContractPrec p (And c1 c2) =
  parensIf (p > 1) $ ppMaybeParens 1 c1 ++ " and " ++ ppMaybeParens 1 c2

ppContractPrec p (Then c1 c2) =
  parensIf (p > 2) $ ppMaybeParens 2 c1 ++ " then " ++ ppMaybeParens 2 c2

ppContractPrec p (Give c)    = parensIf (p > 3) $ "give "    ++ ppContractPrec 3 c
ppContractPrec p (Scale o c) = parensIf (p > 3) $ "scale "   ++ ppObs o ++ " " ++ ppContractPrec 3 c
ppContractPrec p (Truncate d c) = parensIf (p > 3) $ "truncate " ++ ppDate d ++ " " ++ ppContractPrec 3 c

-- | if <cond> then <c1> else <c2>
ppContractPrec p (If cond c1 c2) =
  parensIf (p > 3) $
    "if " ++ ppObsBool cond
    ++ " then " ++ ppContractPrec 3 c1
    ++ " else " ++ ppContractPrec 3 c2

ppMaybeParens :: Int -> Contract -> String
ppMaybeParens prec child
  | isInfix child && infixPrec child /= prec = "(" ++ ppContractPrec 0 child ++ ")"
  | otherwise                                = ppContractPrec (prec + 1) child

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

-- ─── Observables booleanos ────────────────────────────────────────────────────

ppObsBool :: ObsBool -> String
ppObsBool (Gt  a b) = ppObs a ++ " > "  ++ ppObs b
ppObsBool (Lt  a b) = ppObs a ++ " < "  ++ ppObs b
ppObsBool (Gte a b) = ppObs a ++ " >= " ++ ppObs b
ppObsBool (Lte a b) = ppObs a ++ " <= " ++ ppObs b
ppObsBool (Eq  a b) = ppObs a ++ " == " ++ ppObs b

-- ─── Observables numéricos ────────────────────────────────────────────────────

ppObs :: Show a => Obs a -> String
ppObs = ppObsPrec 0

ppObsPrec :: Show a => Int -> Obs a -> String
ppObsPrec _ (Konst x)       = showNum x
ppObsPrec _ (External s)    = s
ppObsPrec _ (Balance p cur) = "balance " ++ p ++ " " ++ ppCurrency cur
ppObsPrec p (Add a b) = parensIf (p > 0) $ ppObsPrec 0 a ++ " + " ++ ppObsPrec 1 b
ppObsPrec p (Sub a b) = parensIf (p > 0) $ ppObsPrec 0 a ++ " - " ++ ppObsPrec 1 b
ppObsPrec p (Mul a b) = parensIf (p > 1) $ ppObsPrec 1 a ++ " * " ++ ppObsPrec 2 b
ppObsPrec p (Div a b) = parensIf (p > 1) $ ppObsPrec 1 a ++ " / " ++ ppObsPrec 2 b
ppObsPrec p (Neg a)   = parensIf (p > 1) $ "-" ++ ppObsPrec 2 a

-- ─── Comandos ─────────────────────────────────────────────────────────────────

ppComm :: Comm -> String
ppComm (Assign name c)         = "let " ++ name ++ " = " ++ ppContract c
ppComm (Run c)                 = ppContract c
ppComm (Seq c1 c2)             = ppComm c1 ++ ";\n" ++ ppComm c2
ppComm (Deposit party cur amt) = "deposit " ++ party ++ " " ++ showDouble amt ++ " " ++ ppCurrency cur
ppComm (Propose name c)        = "propose " ++ name ++ " " ++ ppContract c
ppComm (Sign name party)       = "sign " ++ name ++ " " ++ party
ppComm (Execute name)          = "execute " ++ name
ppComm (SetFecha d)            = "setfecha " ++ ppDate d

-- ─── Cashflows ────────────────────────────────────────────────────────────────

ppCashflow :: Cashflow -> String
ppCashflow cf =
  ppDate (fecha cf)
  ++ "  " ++ ppAmount (cantidad cf)
  ++ "  " ++ desde cf ++ " → " ++ hacia cf

ppAmount :: Amount -> String
ppAmount (Amount v c) = showDouble v ++ " " ++ ppCurrency c

ppCashflows :: [Cashflow] -> String
ppCashflows [] = "(sin flujos de caja)"
ppCashflows cfs =
  let header = "Fecha        Monto          Desde → Hacia"
      sep    = replicate (length header) '-'
  in  unlines (header : sep : map ppCashflow cfs)

-- ─── Historial ────────────────────────────────────────────────────────────────

ppHistorial :: [HistorialEntry] -> String
ppHistorial [] = "(sin ejecuciones registradas)"
ppHistorial hs = unlines $ map ppHistorialEntry hs

ppHistorialEntry :: HistorialEntry -> String
ppHistorialEntry h =
  "  [" ++ ppDate (hFecha h) ++ "] " ++ hNombre h
  ++ "  (" ++ hPartyA h ++ " / " ++ hPartyB h ++ ")\n"
  ++ unlines (map (("    " ++) . ppCashflow) (hCashflows h))

-- ─── Billeteras ───────────────────────────────────────────────────────────────

ppWallets :: Wallets -> String
ppWallets ws
  | Map.null ws = "(sin billeteras registradas)"
  | otherwise   = unlines $ concatMap ppPartyWallet (Map.toList ws)

ppPartyWallet :: (PartyId, Map Currency Double) -> [String]
ppPartyWallet (party, balances) =
  let header = "  [" ++ party ++ "]"
      sep    = "  " ++ replicate 30 '-'
      rows   = map ppBalEntry (Map.toList balances)
      total  = if null rows then ["    (sin fondos)"] else rows
  in  header : sep : total ++ [""]

ppWallet :: Wallets -> PartyId -> String
ppWallet ws party =
  case Map.lookup party ws of
    Nothing  -> "  " ++ party ++ ": (sin billetera)"
    Just bal ->
      let rows = map ppBalEntry (Map.toList bal)
      in  unlines $ ("  [" ++ party ++ "]") : ("  " ++ replicate 30 '-') : rows

ppBalEntry :: (Currency, Double) -> String
ppBalEntry (cur, bal) = "    " ++ padR 6 (ppCurrency cur) ++ showDouble bal

-- ─── Contratos pendientes ─────────────────────────────────────────────────────

ppPendings :: Map String PendingContract -> String
ppPendings store
  | Map.null store = "(sin contratos pendientes)"
  | otherwise      = unlines $ map ppPending (Map.toList store)

ppPending :: (String, PendingContract) -> String
ppPending (name, pc) =
  let statusA    = if pcSignA pc == Signed then "[firmado]" else "[pendiente]"
      statusB    = if pcSignB pc == Signed then "[firmado]" else "[pendiente]"
      fullStatus = if isFullySigned pc then " (listo para ejecutar)" else ""
  in  "  " ++ name ++ fullStatus ++ "\n"
      ++ "     Contrato: " ++ ppContract (pcContract pc) ++ "\n"
      ++ "     Creado:   " ++ ppDate (pcCreatedAt pc)    ++ "\n"
      ++ "     " ++ pcPartyA pc ++ " " ++ statusA
      ++ "  |  " ++ pcPartyB pc ++ " " ++ statusB

-- ─── Errores ──────────────────────────────────────────────────────────────────

ppError :: EvalError -> String
ppError DivByZero      = "ERROR (DivByZero): división por cero"
ppError (UnknownObs s) = "ERROR (UnknownObs): observable desconocido '" ++ s ++ "'"
ppError (EvalMsg s)    = "ERROR (EvalMsg): " ++ s

-- ─── Primitivas ───────────────────────────────────────────────────────────────

ppCurrency :: Currency -> String
ppCurrency USD = "USD"
ppCurrency EUR = "EUR"
ppCurrency ARS = "ARS"
ppCurrency GBP = "GBP"

ppDate :: Day -> String
ppDate = show

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

showNum :: Show a => a -> String
showNum = show

showDouble :: Double -> String
showDouble x
  | x == fromIntegral (round x :: Integer) = show (round x :: Integer)
  | otherwise                               = show x

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '