module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad                  ( when )
import           Control.Monad.Trans.Class      ( lift )
import           Data.Char
import           Data.List
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.Parsec                    ( parse )
import           Data.Time                      ( fromGregorian, getCurrentTime, utctDay )

import           Utils
import qualified Data.Map as Map
import           AST
import           Types
import           Parser
import           PrettyPrinter
import           Evaluator
import           Monads

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  today <- lift $ utctDay <$> getCurrentTime
  
  let defaultQuotes = Map.fromList
        [ ("OIL", 80.5)
        , ("AAPL", 150.0)
        , ("USD_ARS", 1000.0)
        ]
  
  let defaultEnv = Env 
        { fechaHoy    = today
        , getQuote     = makeOracle defaultQuotes
        , yo           = "Bussa"
        , contraparte  = "Banco"
        }
  
  let initialState = S { inter = False, lfile = "", fEnv = defaultEnv, cStore = Map.empty, qStore = defaultQuotes }
  
  readevalprint args initialState

-- | Construir función oráculo a partir del mapa de cotizaciones
makeOracle :: Map.Map String Double -> (String -> Double)
makeOracle quotes name = case Map.lookup name quotes of
    Just v  -> v
    Nothing -> 1.0  -- Valor por defecto para observables desconocidos

iname, iprompt :: String
iname = "EDSL de Contratos Financieros"
iprompt = "Fin> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  { inter  :: Bool
  , lfile  :: String
  , fEnv   :: Env
  , cStore :: Evaluator.ContractStore  -- Diccionario con las variables de contrato
  , qStore :: Map.Map String Double    -- Cotizaciones del oráculo
  }

readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state =
  let rec st = do
        mx <- MC.catch
          (if inter st then getInputLine iprompt else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            cmd <- interpretCommand x
            st' <- handleCommand st cmd
            maybe (return ()) rec st'
  in  do
        -- Si hubiera archivos que pasar por argumento, se compilarían aquí
        -- state' <- compileFiles args state 
        when (not (inter state)) $ lift $ putStrLn
          (  "Intérprete de "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        rec state { inter = True }

data Command = EvalContract String
             | EvalComm String
             | PrintAST String
             | PrintPP String
             | LoadFile String
             | ShowStore
             | SetQuote String
             | Quit
             | Help
             | Noop

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x
    let t         = dropWhile isSpace t'
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
        return Noop
      [Cmd _ _ f _] -> return (f t)
      _ -> do
        putStrLn "Comando ambigüo."
        return Noop
  else return (EvalComm x)

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state cmd = case cmd of
  Quit   -> lift $ putStrLn "Saliendo del intérprete financiero." >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  
  PrintAST s -> do
    case parse parserContract "<interactive>" s of
      Left err -> lift $ putStrLn $ "Error de parseo: " ++ show err
      Right ast -> lift $ putStrLn $ "AST:\n" ++ show ast
    return (Just state)

  PrintPP s -> do
    case parse parserComm "<interactive>" s of
      Left err -> lift $ putStrLn $ "Error de parseo: " ++ show err
      Right comm -> lift $ putStrLn $ ppComm comm
    return (Just state)

  ShowStore -> do
    lift $ do
      let store = cStore state
      if Map.null store
        then putStrLn "(store vacío - no hay contratos definidos)"
        else mapM_ (\(name, c) -> putStrLn $ "  " ++ name ++ " = " ++ ppContract c)
                   (Map.toList store)
    return (Just state)

  LoadFile path -> do
    let path' = dropWhile (== ' ') path
    if null path'
      then do
        lift $ putStrLn "Uso: :load <archivo.fin>"
        return (Just state)
      else do
        result <- lift $ catch (Right <$> readFile path')
                               (\e -> return $ Left (show (e :: IOException)))
        case result of
          Left err -> do
            lift $ putStrLn $ "Error leyendo archivo: " ++ err
            return (Just state)
          Right content -> do
            -- Filtrar comentarios y líneas vacías, unir con espacio
            -- (los ; deben estar explícitos en el archivo .fin)
            let lns = filter (not . isCommentOrEmpty) (lines content)
            let input = intercalate " " lns
            case parse parserComm path' input of
              Left err -> do
                lift $ putStrLn $ "Error de parseo en " ++ path' ++ ": " ++ show err
                return (Just state)
              Right comm -> do
                case evalComm (cStore state) (fEnv state) comm of
                  Left err -> do
                    lift $ putStrLn $ ppError err
                    return (Just state)
                  Right (newStore, cashflows) -> do
                    lift $ do
                      putStrLn $ "Archivo " ++ path' ++ " cargado correctamente."
                      when (not (null cashflows)) $ putStr $ ppCashflows cashflows
                    return (Just state { cStore = newStore })
    
  SetQuote s -> do
    let ws = words s
    case ws of
      [] -> do
        lift $ do
          let quotes = qStore state
          if Map.null quotes
            then putStrLn "(sin cotizaciones definidas)"
            else do
              putStrLn "Cotizaciones actuales:"
              mapM_ (\(name, v) -> putStrLn $ "  " ++ name ++ " = " ++ show v)
                    (Map.toList quotes)
        return (Just state)
      [name, val] -> case reads val of
        [(v, "")] -> do
          let newQuotes = Map.insert name v (qStore state)
          let newEnv = (fEnv state) { getQuote = makeOracle newQuotes }
          lift $ putStrLn $ "Cotización actualizada: " ++ name ++ " = " ++ show v
          return (Just state { qStore = newQuotes, fEnv = newEnv })
        _ -> do
          lift $ putStrLn $ "Valor inválido: " ++ val ++ ". Debe ser un número."
          return (Just state)
      _ -> do
        lift $ putStrLn "Uso: :quote <nombre> <valor>  (ej: :quote OIL 85.0)"
        return (Just state)

  EvalComm s -> do
    case parse parserComm "<interactive>" s of
      Left err -> do
        lift $ putStrLn $ "Error de parseo: " ++ show err
        return (Just state)
      Right comm -> do
        case evalComm (cStore state) (fEnv state) comm of
          Left err -> do
            lift $ putStrLn $ ppError err
            return (Just state)
          Right (newStore, cashflows) -> do
            lift $ do
              if null cashflows
                then when (hasAssign comm) $
                       putStrLn "Contrato(s) asignado(s) correctamente."
                else putStr $ ppCashflows cashflows
            return (Just state { cStore = newStore })

  EvalContract s -> do
    case parse parserContract "<interactive>" s of
      Left err -> lift $ putStrLn $ "Error de parseo: " ++ show err
      Right contrato -> lift $ do
        case substContract (cStore state) contrato of
          Left err -> putStrLn $ ppError err
          Right resolved -> do
            putStrLn $ "Evaluando: " ++ ppContract resolved
            case runEval (evalContract resolved) (fEnv state) of
              Left err -> putStrLn $ ppError err
              Right ((), cashflows) -> putStr $ ppCashflows cashflows
    return (Just state)

data InteractiveCommand = Cmd [String] String (String -> Command) String

hasAssign :: Comm -> Bool
hasAssign (Assign _ _) = True
hasAssign (Seq c1 c2)  = hasAssign c1 || hasAssign c2
hasAssign (Run _)     = False

-- | Determina si una línea es un comentario (empieza con --) o está vacía
isCommentOrEmpty :: String -> Bool
isCommentOrEmpty s =
    let trimmed = dropWhile isSpace s
    in null trimmed || isPrefixOf "--" trimmed

commands :: [InteractiveCommand]
commands =
  [ Cmd [":ast"]    "<exp>"          PrintAST     "Muestra el AST de un contrato"
  , Cmd [":pp"]     "<exp>"          PrintPP      "Pretty print de un contrato/comando"
  , Cmd [":eval"]   "<exp>"          EvalContract "Evalúa un contrato y muestra flujos de caja"
  , Cmd [":load"]   "<archivo>"      LoadFile     "Cargar un archivo .fin"
  , Cmd [":store"]  ""               (const ShowStore)  "Mostrar contratos definidos"
  , Cmd [":quote"]  "[<nombre> <val>]" SetQuote   "Ver/definir cotización (ej: :quote OIL 85.0)"
  , Cmd [":quit"]   ""               (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] ""           (const Help) "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<contrato>              evaluar y mostrar flujos de caja\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let
               ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )