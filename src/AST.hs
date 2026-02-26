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
  | Var String
  | If ObsBool Contract Contract
  deriving (Show, Eq)

-- ─── Observables booleanos ────────────────────────────────────────────────────
-- Tipo separado de Obs Double para mantener el sistema de tipos limpio.
-- Las condiciones solo soportan comparaciones numéricas por ahora.

data ObsBool
  = Gt  (Obs Double) (Obs Double)   -- obs > obs
  | Lt  (Obs Double) (Obs Double)   -- obs < obs
  | Gte (Obs Double) (Obs Double)   -- obs >= obs
  | Lte (Obs Double) (Obs Double)   -- obs <= obs
  | Eq  (Obs Double) (Obs Double)   -- obs == obs
  deriving (Show, Eq)

-- ─── Comandos ─────────────────────────────────────────────────────────────────

data Comm
  = Assign  String Contract          -- let x = <contrato>
  | Seq     Comm Comm                -- c1 ; c2
  | Run     Contract                 -- evalúa el contrato directamente
  | Deposit PartyId Currency Double  -- deposit <parte> <monto> <moneda>
  | Propose String Contract          -- propose <nombre> <contrato>
  | Sign    String PartyId           -- sign <nombre> <parte>
  | Execute  String                  -- execute <nombre>
  | SetFecha Date                    -- setfecha YYYY-MM-DD
  deriving (Show, Eq)

-- ─── Observables numéricos ────────────────────────────────────────────────────

data Obs a
  = Konst    a
  | Add      (Obs a) (Obs a)
  | Sub      (Obs a) (Obs a)
  | Mul      (Obs a) (Obs a)
  | Div      (Obs a) (Obs a)
  | Neg      (Obs a)
  | External String
  | Balance  PartyId Currency
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

-- ─── Contratos pendientes de firma ────────────────────────────────────────────

data PendingContract = PendingContract
  { pcName      :: String
  , pcContract  :: Contract
  , pcPartyA    :: PartyId
  , pcPartyB    :: PartyId
  , pcSignA     :: SignatureStatus
  , pcSignB     :: SignatureStatus
  , pcCreatedAt :: Day
  } deriving (Show)

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