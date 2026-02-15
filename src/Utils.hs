module Utils where

import AST
import Types

zcb :: Date -> Double -> Currency -> Contract
zcb t x c = Scale (Konst x) (Get (Truncate t (One c)))

