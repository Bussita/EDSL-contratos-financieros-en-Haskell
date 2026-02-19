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
  args <- lift getArgs -- getArgs tokeniza los strings que se le pasan al inicio del programa
  -- por ejemplo podriamos hacer stack run -- Ejemplos/basico.fin
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
  
  readevalprint args initialState -- inicia el loop

-- Construye una función oráculo a partir de el mapa dado
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
-- Inter es una bandera que indica cómo se lee la entrada (abajo en readevalprint se ve el uso)
-- lfile es el acrónimo de "last file" y la idea es que guarde el path del último archivo cargado
-- se implementa con la intención de hacer "reload" (no está implementado)
-- Después tenemos fEnv que es nuestro entorno que definimos en Types.hs
-- cStore es nuestro diccionario con las variables de contrato (definido en Evaluator.hs)
-- qStore es nuestro diccionario de Quotes (osea nuestro Oráculo), que son los valores observables externos.


readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state =
  let rec st = do -- definimos una funcion local rec que es la que hace el loop recursivo
        mx <- MC.catch
          (if inter st then getInputLine iprompt else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
          -- getImputLine es una función de la librería Haskeline que hace que podamos customizar
          -- la entrada estándar con un prompt que elijamos (el nuestro es >Fin )
          -- Cuando la bandera inter es false, se usa el getLine por defecto de IO, el cual
          -- se le aplica un Just por un tema de tipos y se liftea a la transformadora de monadas InputT

          -- MC.catch es control de errores que funciona como una especie de lift para el error en InputT
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            cmd <- interpretCommand x
            st' <- handleCommand st cmd
            maybe (return ()) rec st' -- equivalente a hacer case st' of 
  in  do
        -- Si hubiera archivos que pasar por argumento, se podría agregar esto
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
             | SetPartes String
             | Quit
             | Help
             | Noop

--interpretCommand ve si el comando de la consola comienza con : (y lo busca en tabla)
-- por otro lado si no comienza con : lo trata como expresion del lenguaje
interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x -- se fija si es prefijo
  then do
    let (cmd, t') = break isSpace x -- Toma el prefijo mas largo que no es espacio, es decir, el comando
    let t         = dropWhile isSpace t' -- borra espacios al inicio
    -- la lista commands esta armada con todos elementos de tipo InteractiveCommands asi que
    -- la funcion del filtro matchea
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
    case parse parserContract "<interactive>" s of -- interactive es para indicar que el error vino de REPL
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

  SetPartes s -> do
    let ws = words s
    case ws of
      [] -> do
        lift $ putStrLn $ "Partes actuales: yo = " ++ yo (fEnv state) ++ ", contraparte = " ++ contraparte (fEnv state)
        return (Just state)
      [p1, p2] -> do
        let newEnv = (fEnv state) { yo = p1, contraparte = p2 }
        lift $ putStrLn $ "Partes actualizadas: yo = " ++ p1 ++ ", contraparte = " ++ p2
        return (Just state { fEnv = newEnv })
      _ -> do
        lift $ putStrLn "Uso: :partes <yo> <contraparte>  (ej: :partes Santiago HSBC)"
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
-- InteractiveCommand tiene: Lista de posibles nombres de comando - Descripcion del argumento
-- Funcion que construye el command y un string de ayuda.

hasAssign :: Comm -> Bool
hasAssign (Assign _ _) = True
hasAssign (Seq c1 c2)  = hasAssign c1 || hasAssign c2
hasAssign (Run _)     = False

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
  , Cmd [":partes"] "[<yo> <contra>]" SetPartes  "Ver/cambiar partes (ej: :partes Santiago HSBC)"
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