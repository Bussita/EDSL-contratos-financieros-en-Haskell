module AST where

import Types
import Data.Time (Day)
import GHC.Real (Fractional(fromRational))

-- ─── Contratos ────────────────────────────────────────────────────────────────

data Contract
  = Zero
  | One Currency
  | Give Contract
  | And Contract Contract
  | Or  Contract Contract
  | Truncate Date Contract
  | Then Contract Contract
  | Scale (Obs Double) Contract
  | Get Contract
  | Anytime Contract
  | Var String
  deriving (Show, Eq)

-- ─── Comandos ─────────────────────────────────────────────────────────────────
-- Los nuevos constructores hacen que deposit/propose/sign/execute sean
-- ciudadanos de primera clase del lenguaje, parseables en archivos .fin
-- y ejecutables por evalComm.

data Comm
  = Assign  String Contract          -- let x = <contrato>
  | Seq     Comm Comm                -- c1 ; c2
  | Run     Contract                 -- evalúa el contrato directamente
  -- ── Billeteras ──
  | Deposit PartyId Currency Double  -- deposit <parte> <monto> <moneda>
  -- ── Contratos con firma ──
  | Propose String Contract          -- propose <nombre> <contrato>
  | Sign    String PartyId           -- sign <nombre> <parte>
  | Execute String                   -- execute <nombre>
  deriving (Show, Eq)

-- ─── Observables ──────────────────────────────────────────────────────────────
-- Balance permite consultar saldos desde dentro de un contrato:
--   scale (balance "Alice" USD) one USD
-- Esto hace que los saldos sean parte del lenguaje observable, no solo del REPL.

data Obs a
  = Konst    a
  | Add      (Obs a) (Obs a)
  | Sub      (Obs a) (Obs a)
  | Mul      (Obs a) (Obs a)
  | Div      (Obs a) (Obs a)
  | Neg      (Obs a)
  | External String              -- cotización externa: getQuote del Env
  | Balance  PartyId Currency    -- saldo de una parte en una moneda
  deriving (Show, Eq, Read)

instance Num a => Num (Obs a) where
  (+)         = Add
  (*)         = Mul
  (-)         = Sub
  abs         = error "Obs: abs no implementado"
  signum      = error "Obs: signum no implementado"
  fromInteger = Konst . fromInteger

instance Fractional a => Fractional (Obs a) where
  (/)          = Div
  fromRational = Konst . fromRational

-- Contratos pendientes de firma
-- Viven en el AST porque referencian Contract.

data PendingContract = PendingContract
  { pcName      :: String
  , pcContract  :: Contract
  , pcPartyA    :: PartyId
  , pcPartyB    :: PartyId
  , pcSignA     :: SignatureStatus
  , pcSignB     :: SignatureStatus
  , pcCreatedAt :: Day
  } deriving (Show)

-- Crea un contrato pendiente sin firmas.
createPending :: String -> Contract -> PartyId -> PartyId -> Day -> PendingContract
createPending name contract partyA partyB date = PendingContract
  { pcName      = name
  , pcContract  = contract
  , pcPartyA    = partyA
  , pcPartyB    = partyB
  , pcSignA     = Pending
  , pcSignB     = Pending
  , pcCreatedAt = date
  }

-- Registra la firma de una parte. Falla si la parte no es signataria
-- o si ya había firmado.
signContract :: PartyId -> PendingContract -> Either String PendingContract
signContract party pc
  | party == pcPartyA pc =
      if pcSignA pc == Signed
        then Left $ party ++ " ya firmó este contrato."
        else Right pc { pcSignA = Signed }
  | party == pcPartyB pc =
      if pcSignB pc == Signed
        then Left $ party ++ " ya firmó este contrato."
        else Right pc { pcSignB = Signed }
  | otherwise =
      Left $ party ++ " no es parte de este contrato ("
          ++ pcPartyA pc ++ " / " ++ pcPartyB pc ++ ")."

isFullySigned :: PendingContract -> Bool
isFullySigned pc = pcSignA pc == Signed && pcSignB pc == Signed