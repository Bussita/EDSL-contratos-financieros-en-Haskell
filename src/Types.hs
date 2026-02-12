{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Time (UTCTime, LocalTime, TimeOfDay)
import GHC.Generics (Generic)

-- | Identificador para una transacción o parte
type PartyId = String

-- | Cantidad de dinero con una moneda
data Amount = Amount
  { value :: Double
  , currency :: String
  } deriving (Show, Eq, Generic)

-- | Observable financiero
data Observable = Observable
  { name :: String
  , description :: Maybe String
  } deriving (Show, Eq, Generic)

-- | Resultado de una evaluación
data EvalResult = EvalResult
  { cashflows :: [Cashflow]
  } deriving (Show, Eq)

-- | Flujo de caja
data Cashflow = Cashflow
  { date :: LocalTime
  , amount :: Amount
  , from :: PartyId
  , to :: PartyId
  } deriving (Show, Eq, Generic)