import AST
import Types

zcb :: Date -> Double -> Currency -> Contract
zcb t x c = Scale x (Get (Truncate t (One c)))

