module Wallet where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time (Day)

import Types
import AST
import PrettyPrinter (ppCurrency, ppDate, showDouble, ppContract)

type Wallets = Map PartyId (Map Currency Double)

-- Estado de firma de un contrato pendiente
data SignatureStatus = Pending | Signed deriving (Show, Eq)

-- Un contrato pendiente que requiere firmas para ser ejecutado
data PendingContract = PendingContract
    { pcName       :: String             -- Nombre del contrato pendiente
    , pcContract   :: Contract           -- El contrato en sí
    , pcPartyA     :: PartyId            -- Primera parte (creador)
    , pcPartyB     :: PartyId            -- Segunda parte (contraparte)
    , pcSignA      :: SignatureStatus    -- Firma de la parte A
    , pcSignB      :: SignatureStatus    -- Firma de la parte B
    , pcCreatedAt  :: Day                -- Fecha de creación
    } deriving (Show)

type PendingStore = Map String PendingContract

emptyWallets :: Wallets
emptyWallets = Map.empty

-- Obtiene el saldo de una parte en una moneda
getBalance :: Wallets -> PartyId -> Currency -> Double
getBalance wallets party cur =
    case Map.lookup party wallets of
        Nothing -> 0
        Just balances -> Map.findWithDefault 0 cur balances

-- Deposita fondos en la billetera de una parte
deposit :: PartyId -> Currency -> Double -> Wallets -> Wallets
deposit party cur amount wallets =
    let current = getBalance wallets party cur
        newBal  = current + amount
        partyMap = Map.findWithDefault Map.empty party wallets
    in  Map.insert party (Map.insert cur newBal partyMap) wallets

-- Intenta debitar fondos de una billetera. Falla si no hay saldo suficiente.
withdraw :: PartyId -> Currency -> Double -> Wallets -> Either String Wallets
withdraw party cur amount wallets =
    let current = getBalance wallets party cur
    in  if current < amount
        then Left $ party ++ " no tiene saldo suficiente en " ++ ppCurrency cur
                    ++ " (tiene " ++ showDouble current
                    ++ ", necesita " ++ showDouble amount ++ ")"
        else Right $ deposit party cur (-amount) wallets

-- Aplica una lista de cashflows a las billeteras.
-- Verifica que el pagador tenga fondos suficientes.
applyCashflows :: Wallets -> [Cashflow] -> Either String Wallets
applyCashflows = foldlM applyOne
  where
    applyOne wall cf =
        let payer    = desde cf
            receiver = hacia cf
            cur      = currency (cantidad cf)
            amt      = value (cantidad cf)
        in  case withdraw payer cur amt wall of
                Left err  -> Left err
                Right wall' -> Right $ deposit receiver cur amt wall'

-- foldlM manual
foldlM :: (a -> b -> Either String a) -> a -> [b] -> Either String a
foldlM _ acc []     = Right acc
foldlM f acc (x:xs) = case f acc x of
                         Left err  -> Left err
                         Right acc' -> foldlM f acc' xs

-- Aplica cashflows sin verificar saldo (permite saldos negativos)
applyCashflowsForce :: Wallets -> [Cashflow] -> Wallets
applyCashflowsForce = foldl applyOne
  where
    applyOne ws cf =
        let payer    = desde cf
            receiver = hacia cf
            cur      = currency (cantidad cf)
            amt      = value (cantidad cf)
        in  deposit receiver cur amt (deposit payer cur (negate amt) ws)

createPending :: String -> Contract -> PartyId -> PartyId -> Day
              -> PendingContract
createPending name contract partyA partyB date = PendingContract
    { pcName      = name
    , pcContract  = contract
    , pcPartyA    = partyA
    , pcPartyB    = partyB
    , pcSignA     = Pending
    , pcSignB     = Pending
    , pcCreatedAt = date
    }

-- Firma un contrato pendiente como una parte específica
signContract :: PartyId -> PendingContract -> Either String PendingContract
signContract party pc
    | party == pcPartyA pc =
        if pcSignA pc == Signed
        then Left $ party ++ " ya firmó este contrato."
        else Right $ pc { pcSignA = Signed }
    | party == pcPartyB pc =
        if pcSignB pc == Signed
        then Left $ party ++ " ya firmó este contrato."
        else Right $ pc { pcSignB = Signed }
    | otherwise =
        Left $ party ++ " no es parte de este contrato ("
               ++ pcPartyA pc ++ " / " ++ pcPartyB pc ++ ")."

-- Verifica si un contrato está completamente firmado
isFullySigned :: PendingContract -> Bool
isFullySigned pc = pcSignA pc == Signed && pcSignB pc == Signed

ppWallets :: Wallets -> String
ppWallets wallets
    | Map.null wallets = "(sin billeteras registradas)"
    | otherwise =
        let parties = Map.toList wallets
        in  unlines $ concatMap ppPartyWallet parties

ppPartyWallet :: (PartyId, Map Currency Double) -> [String]
ppPartyWallet (party, balances) =
    let header = "  [" ++ party ++ "]"
        sep    = "  " ++ replicate 30 '-'
        rows   = map ppBalEntry (Map.toList balances)
        total  = if null rows then ["    (sin fondos)"] else rows
    in  header : sep : total ++ [""]

ppBalEntry :: (Currency, Double) -> String
ppBalEntry (cur, bal) =
    "    " ++ padR 6 (ppCurrency cur) ++ showDouble bal

-- Muestra la billetera de una sola parte
ppWallet :: Wallets -> PartyId -> String
ppWallet wallets party =
    case Map.lookup party wallets of
        Nothing -> "  " ++ party ++ ": (sin billetera)"
        Just balances ->
            let rows = map ppBalEntry (Map.toList balances)
            in  unlines $ ("  [" ++ party ++ "]") : ("  " ++ replicate 30 '-') : rows

-- Muestra los contratos pendientes
ppPendings :: PendingStore -> String
ppPendings store
    | Map.null store = "(sin contratos pendientes)"
    | otherwise = unlines $ map ppPending (Map.toList store)

ppPending :: (String, PendingContract) -> String
ppPending (name, pc) =
    let statusA = if pcSignA pc == Signed then "[firmado]" else "[pendiente]"
        statusB = if pcSignB pc == Signed then "[firmado]" else "[pendiente]"
        fullStatus = if isFullySigned pc then " (listo para ejecutar)" else ""
    in  "  " ++ name ++ fullStatus ++ "\n"
        ++ "     Contrato: " ++ ppContract (pcContract pc) ++ "\n"
        ++ "     Creado:   " ++ ppDate (pcCreatedAt pc) ++ "\n"
        ++ "     " ++ pcPartyA pc ++ " " ++ statusA
        ++ "  |  " ++ pcPartyB pc ++ " " ++ statusB

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
