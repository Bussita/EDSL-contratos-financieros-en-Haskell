module Main where

import AST
import Parser
import Evaluator
import Types
import Text.Parsec (parse)

main :: IO ()
main = do
  putStrLn "Contratos financieros EDSL"
  putStrLn "========================"
  putStrLn ""
  
  -- Ejemplo simple
  putStrLn "Prueba de parser (Zero contract):"
  case parse parseContract "" "zero" of
    Left err -> print err
    Right contract -> putStrLn $ "Parseado: " ++ show contract
  
  putStrLn ""
  putStrLn "Prueba de evaluación de expresión:"
  case evalExpression (Add (Const 5) (Const 3)) [] of
    Left err -> putStrLn $ "Error: " ++ err
    Right val -> putStrLn $ "Resultado: " ++ show val
