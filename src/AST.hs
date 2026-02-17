module AST where

import Types
import GHC.Real (Fractional(fromRational))

data Contract =
    Zero |
    One Currency |
    Give Contract |
    And Contract Contract |
    Or Contract Contract |
    Truncate Date Contract |
    Then Contract Contract |
    Scale (Obs Double) Contract |
    Get Contract |
    Anytime Contract |
    Var String
    deriving (Show, Eq)

data Comm =
    Assign String Contract
  | Seq Comm Comm
  | Run Contract
  deriving (Show, Eq)

data Obs a =
    Konst a |
    Add (Obs a) (Obs a) |
    Sub (Obs a) (Obs a) |
    Mul (Obs a) (Obs a) |
    Div (Obs a) (Obs a) |
    Neg (Obs a)         |
    External String deriving (Show, Eq, Read)

instance Num a => Num (Obs a) where
    (+)         = Add
    (*)         = Mul
    (-)         = Sub
    abs         = error "No implementado"
    signum      = error "No implementado"
    fromInteger = Konst . fromInteger

instance Fractional a => Fractional (Obs a) where
    (/) = Div
    fromRational = Konst . fromRational