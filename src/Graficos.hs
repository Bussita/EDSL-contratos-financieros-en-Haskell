module Graficos where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sortBy)
import Data.Ord (comparing)

import Types
import PrettyPrinter (ppCurrency, showDouble, ppAmount, ppDate)

ppGrafico :: PartyId -> [Cashflow] -> String
ppGrafico party cfs
    | null cfs  = "(sin flujos de caja)"
    | otherwise =
        unlines [ "Flujos de caja:"
                , ppFlujos cfs
                , "Grafico (perspectiva de " ++ party ++ "):"
                , ppBarras party cfs
                , "Neto por moneda:"
                , ppResumen party cfs
                ]

ppFlujos :: [Cashflow] -> String
ppFlujos cfs =
    let sorted = sortBy (comparing fecha) cfs
    in  unlines $ map ppFlecha sorted

ppFlecha :: Cashflow -> String
ppFlecha cf =
    "  " ++ ppDate (fecha cf) ++ "  "
    ++ padRight 12 (desde cf)
    ++ " --[ " ++ ppAmount (cantidad cf) ++ " ]--> "
    ++ hacia cf

ppBarras :: PartyId -> [Cashflow] -> String
ppBarras party cfs =
    let entries = map (cfToEntry party) cfs
        maxAbs  = if null entries then 1
                  else maximum (map (abs . fst) entries)
        sc      = if maxAbs == 0 then 1 else 40.0 / maxAbs
    in  unlines $ map (ppBarra sc) entries

ppBarra :: Double -> (Double, String) -> String
ppBarra sc (val, label) =
    let barLen = round (abs val * sc) :: Int
        bar    = if val >= 0
                 then replicate barLen '#'
                 else replicate barLen '='
    in  "  " ++ padRight 20 label ++ " " ++ bar

ppResumen :: PartyId -> [Cashflow] -> String
ppResumen party cfs =
    let netMap = foldl (accumNet party) Map.empty cfs
    in  unlines $ map ppNetRow (Map.toList netMap)

ppNetRow :: (Currency, Double) -> String
ppNetRow (cur, net) =
    let sign = if net >= 0 then "+" else ""
    in  "  " ++ padRight 6 (ppCurrency cur) ++ sign ++ showDouble net

cfToEntry :: PartyId -> Cashflow -> (Double, String)
cfToEntry party cf
    | hacia cf == party =
        (v, "+" ++ showDouble v ++ " " ++ ppCurrency cur)
    | desde cf == party =
        (negate v, "-" ++ showDouble v ++ " " ++ ppCurrency cur)
    | otherwise = (0, "0 " ++ ppCurrency cur)
  where
    v   = value (cantidad cf)
    cur = currency (cantidad cf)

accumNet :: PartyId -> Map Currency Double -> Cashflow -> Map Currency Double
accumNet party acc cf =
    let cur = currency (cantidad cf)
        v   = value (cantidad cf)
        delta = if hacia cf == party then v
                else if desde cf == party then negate v
                else 0
    in  Map.insertWith (+) cur delta acc

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '
