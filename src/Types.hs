{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)

type Date = Day

data Currency = USD | EUR | ARS | GBP deriving (Show, Eq, Read, Ord)

type PartyId = String

data Amount = Amount
  { value :: Double
  , currency :: Currency
  } deriving (Show, Eq, Generic)

data Cashflow = Cashflow
  { fecha :: Day
  , cantidad :: Amount
  , desde :: PartyId
  , hacia :: PartyId
  } deriving (Show, Eq, Generic)

data EvalError = DivByZero | UnknownObs String | EvalMsg String deriving (Show, Eq)

data Env = Env { fechaHoy    :: Date
               , getQuote    :: String -> Maybe Double
               , yo          :: PartyId
               , contraparte :: PartyId
               }