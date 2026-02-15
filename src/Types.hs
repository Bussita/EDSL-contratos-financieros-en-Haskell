{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)

-- | Tipo para representar una fecha
type Date = Day

-- | Tipo para representar una moneda
data Currency = USD | EUR | ARS | GBP deriving (Show, Eq, Read)

-- | Identificador para una transacción o parte
type PartyId = String

-- | Cantidad de dinero con una moneda
data Amount = Amount
  { value :: Double
  , currency :: String
  } deriving (Show, Eq, Generic)

-- | Resultado de una evaluación
data EvalResult = EvalResult
  { cashflows :: [Cashflow]
  } deriving (Show, Eq)

-- | Flujo de caja
data Cashflow = Cashflow
  { date :: Day
  , amount :: Amount
  , from :: PartyId
  , to :: PartyId
  } deriving (Show, Eq, Generic)