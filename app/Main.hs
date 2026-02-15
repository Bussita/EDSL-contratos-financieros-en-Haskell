module Main where

import AST
import Parser
import Types
import Text.Parsec (parse)

-- Función auxiliar para imprimir bonito los resultados
probar :: String -> IO ()
probar input = do
  putStrLn $ "Input:  " ++ input
  case parse parserContract "" input of
    Left err -> putStrLn $ "ERROR:  " ++ show err
    Right contract -> do
        putStrLn "ÉXITO:  Parseado correctamente."
        putStrLn $ "AST:    " ++ show contract
  putStrLn "---------------------------------------------------"

main :: IO ()
main = do
  putStrLn "\n=== TEST SUITE: PARSER DE CONTRATOS ===\n"

  -- 1. Primitivas Básicas
  putStrLn "-- 1. Primitivas --"
  probar "zero"
  probar "one USD"
  probar "one ARS"

  -- 2. Observables (Matemáticas y Variables)
  putStrLn "\n-- 2. Escalar con Observables --"
  -- Escalar por una constante
  probar "scale 100 (one USD)"
  
  -- Escalar por una variable externa (ej: precio del petróleo)
  probar "scale OIL (one USD)"
  
  -- Escalar por una operación matemática (10% más)
  probar "scale (1.1 * PRICE) (one USD)"

  -- 3. Fechas (Truncate)
  putStrLn "\n-- 3. Fechas y Límites --"
  -- Formato YYYY-MM-DD
  probar "truncate 2025-12-31 (one USD)"
  
  -- Combinación de fecha y escala
  probar "truncate 2024-01-01 (scale 10 one EUR)"

  -- 4. Operadores Lógicos (And, Or, Give)
  putStrLn "\n-- 4. Combinadores Lógicos --"
  probar "one USD and one EUR"
  probar "one USD or zero"
  probar "give (one USD)"

  -- 5. Secuenciación (Then)
  putStrLn "\n-- 5. Secuenciación --"
  probar "one USD then one EUR"

  -- 6. Precedencia Compleja (La prueba de fuego)
  putStrLn "\n-- 6. Contratos Complejos (Precedencia) --"
  
  -- Objetivo: 'scale' debe pegar más fuerte que 'and'
  -- Debería ser: (scale 10 one USD) AND (one EUR)
  -- NO debería ser: scale 10 (one USD AND one EUR)
  probar "scale 10 one USD and one EUR"
  
  -- Objetivo: 'and' debe pegar más fuerte que 'or'
  probar "one USD and one EUR or one GBP"

  -- Un contrato "realista": Un bono cupón cero
  -- "Recibe 100 USD en la fecha tope"
  putStrLn "\n-- 7. Ejemplo 'Zero Coupon Bond' --"
  probar "get (truncate 2026-01-01 (scale 100 one USD))"