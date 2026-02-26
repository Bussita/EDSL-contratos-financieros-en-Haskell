{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Time (Day)
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Generics (Generic)

type Date = Day

data Currency = USD | EUR | ARS | GBP deriving (Show, Eq, Read, Ord)

type PartyId = String

data Amount = Amount
  { value    :: Double
  , currency :: Currency
  } deriving (Show, Eq, Generic)

data Cashflow = Cashflow
  { fecha    :: Day
  , cantidad :: Amount
  , desde    :: PartyId
  , hacia    :: PartyId
  } deriving (Show, Eq, Generic)

data EvalError
  = DivByZero
  | UnknownObs String
  | EvalMsg String
  deriving (Show, Eq)

-- | Entorno de solo lectura que los contratos consultan durante la evaluación.
-- Se pasa vía ReaderT en la mónada Interp.
data Env = Env
  { fechaHoy    :: Date
  , getQuote    :: String -> Maybe Double
  , yo          :: PartyId
  , contraparte :: PartyId
  }

-- | Saldos de todas las partes: PartyId → (Currency → monto).
type Wallets = Map PartyId (Map Currency Double)

-- | Estado de firma de un contrato pendiente.
data SignatureStatus = Pending | Signed deriving (Show, Eq)

-- | Una entrada en el historial de ejecuciones.
--   Se registra cada vez que `execute` termina exitosamente.
data HistorialEntry = HistorialEntry
  { hNombre    :: String       -- nombre del contrato ejecutado
  , hFecha     :: Day          -- fecha de ejecución (fechaHoy del Env)
  , hPartyA    :: PartyId      -- parte A del contrato
  , hPartyB    :: PartyId      -- parte B del contrato
  , hCashflows :: [Cashflow]   -- flujos de caja generados
  , hWallets   :: Wallets      -- estado de billeteras DESPUÉS de la ejecución
  } deriving (Show, Eq, Generic)

-- | Snapshot del estado de las billeteras en un momento dado.
--   Se registra con cada setfecha y execute para poder graficar la evolución.
data WalletSnapshot = WalletSnapshot
  { snapFecha   :: Day
  , snapWallets :: Wallets
  , snapEvento  :: String      -- descripción del evento que causó el snapshot
  } deriving (Show, Eq, Generic)