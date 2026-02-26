module Main where

import           Control.Exception   (catch, IOException)
import           Control.Monad       (when)
import           Control.Monad.Trans.Class (lift)
import           Data.Char           (isSpace)
import           Data.List           (intercalate, isPrefixOf)
import           System.Console.Haskeline
import qualified Control.Monad.Catch as MC
import           System.Environment  (getArgs)
import           System.IO           hiding (print)
import           Text.Parsec         (parse)
import           Data.Time           (getCurrentTime, utctDay)

import qualified Data.Map.Strict as Map

import AST
import Types
import Parser
import PrettyPrinter
import Evaluator
import Monads
import Exportar

-- ─── Punto de entrada ─────────────────────────────────────────────────────────

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args  <- lift getArgs
  today <- lift $ utctDay <$> getCurrentTime

  let defaultQuotes = Map.fromList
        [ ("OIL",     80.5)
        , ("AAPL",   150.0)
        , ("USD_ARS", 1000.0)
        , ("EUR_USD", 1.08)
        , ("GBP_USD", 1.27)
        , ("ARS_USD", 0.001)
        ]

  let defaultEnv = Env
        { fechaHoy    = today
        , getQuote    = makeOracle defaultQuotes
        , yo          = "Bussa"
        , contraparte = "Banco"
        }

  let initialState = (emptyInterpState today) { isQuotes = defaultQuotes }

  readevalprint args (initialState, defaultEnv)

makeOracle :: Map.Map String Double -> (String -> Maybe Double)
makeOracle qs name = Map.lookup name qs

iname, iprompt :: String
iname   = "EDSL de Contratos Financieros"
iprompt = "Fin> "

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

-- ─── Estado del REPL ──────────────────────────────────────────────────────────
-- El estado del intérprete es (InterpState, Env).
-- InterpState  → lo mutable del lenguaje (wallets, contratos, pending, cotizaciones, fecha)
-- Env          → lo que Interp lee con `ask` (oráculo, partes actuales)
--
-- Los comandos REPL que modifican cotizaciones o partes actualizan el Env
-- directamente; los demás sólo modifican InterpState vía runInterp.

type ReplState = (InterpState, Env)

-- ─── Bucle REPL ───────────────────────────────────────────────────────────────

readevalprint :: [String] -> ReplState -> InputT IO ()
readevalprint _args st0 =
  let rec st = do
        mx <- MC.catch
          (getInputLine iprompt)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            cmd  <- interpretCommand x
            mst' <- handleCommand st cmd
            maybe (return ()) rec mst'
  in do
    lift $ putStrLn ("Intérprete de " ++ iname ++ ".\nEscriba :? para recibir ayuda.")
    rec st0

-- ─── Comandos ─────────────────────────────────────────────────────────────────

data Command
  = EvalComm     String
  | EvalContract String
  | PrintAST     String
  | PrintPP      String
  | LoadFile     String
  | ShowStore
  | SetQuote     String
  | SetPartes    String
  | ShowWallet   String
  | ShowPending
  | ShowHistorial
  | ReplSetFecha String
  | ExportHTML   String
  | ExportASTSVG String
  | Quit
  | Help
  | Noop

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $
  if isPrefixOf ":" x
    then do
      let (cmd, t') = break isSpace x
          t         = dropWhile isSpace t'
          matching  = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
      case matching of
        []              -> putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                           >> return Noop
        [Cmd _ _ f _]  -> return (f t)
        _               -> putStrLn "Comando ambigüo." >> return Noop
    else return (EvalComm x)

-- ─── Manejo de comandos ───────────────────────────────────────────────────────

handleCommand :: ReplState -> Command -> InputT IO (Maybe ReplState)
handleCommand _   Quit = lift (putStrLn "Saliendo del intérprete financiero.") >> return Nothing
handleCommand st  Noop = return (Just st)
handleCommand st  Help = lift (putStr (helpTxt commands)) >> return (Just st)

handleCommand st (PrintAST s) = do
  let ist = fst st
  case parse parserContract "<interactive>" s of
    Left err  -> lift $ putStrLn $ "Error de parseo: " ++ show err
    Right ast -> case substContract (isContracts ist) ast of
      Left err       -> lift $ putStrLn $ ppError err
      Right resolved -> lift $ putStrLn $ "AST:\n" ++ show resolved
  return (Just st)

handleCommand st (PrintPP s) = do
  case parse parserComm "<interactive>" s of
    Left err   -> lift $ putStrLn $ "Error de parseo: " ++ show err
    Right comm -> lift $ putStrLn $ ppComm comm
  return (Just st)

handleCommand st ShowStore = do
  let cs = isContracts (fst st)
  lift $ if Map.null cs
    then putStrLn "(store vacío - no hay contratos definidos)"
    else mapM_ (\(n,c) -> putStrLn $ "  " ++ n ++ " = " ++ ppContract c) (Map.toList cs)
  return (Just st)

handleCommand st ShowPending = do
  lift $ putStrLn $ ppPendings (isPending (fst st))
  return (Just st)

handleCommand st (ShowWallet s) = do
  let ws = isWallets (fst st)
  lift $ case words s of
    []      -> putStrLn $ ppWallets ws
    [party] -> putStrLn $ ppWallet ws party
    _       -> putStrLn "Uso: :wallet [<parte>]"
  return (Just st)

handleCommand (ist, env) (SetQuote s) = do
  case words s of
    [] -> do
      lift $ do
        let qs = isQuotes ist
        if Map.null qs
          then putStrLn "(sin cotizaciones definidas)"
          else do
            putStrLn "Cotizaciones actuales:"
            mapM_ (\(n,v) -> putStrLn $ "  " ++ n ++ " = " ++ show v) (Map.toList qs)
      return (Just (ist, env))
    [name, val] -> case reads val of
      [(v, "")] -> do
        let newQs  = Map.insert name v (isQuotes ist)
            newEnv = env { getQuote = makeOracle newQs }
            newIst = ist { isQuotes = newQs }
        lift $ putStrLn $ "Cotización actualizada: " ++ name ++ " = " ++ show v
        return (Just (newIst, newEnv))
      _ -> lift (putStrLn $ "Valor inválido: " ++ val) >> return (Just (ist, env))
    _ -> lift (putStrLn "Uso: :quote <nombre> <valor>  (ej: :quote OIL 85.0)")
         >> return (Just (ist, env))

handleCommand (ist, env) (SetPartes s) = do
  case words s of
    [] -> do
      lift $ putStrLn $ "Partes actuales: yo = " ++ yo env ++ ", contraparte = " ++ contraparte env
      return (Just (ist, env))
    [p1, p2] -> do
      let newEnv = env { yo = p1, contraparte = p2 }
      lift $ putStrLn $ "Partes actualizadas: yo = " ++ p1 ++ ", contraparte = " ++ p2
      return (Just (ist, newEnv))
    _ -> lift (putStrLn "Uso: :partes <yo> <contraparte>  (ej: :partes Santiago HSBC)")
         >> return (Just (ist, env))

handleCommand st (LoadFile path) = do
  let path' = dropWhile (== ' ') path
  if null path'
    then lift (putStrLn "Uso: :load <archivo.fin>") >> return (Just st)
    else do
      result <- lift $ catch (Right <$> readFile path')
                             (\e -> return $ Left (show (e :: IOException)))
      case result of
        Left err -> lift (putStrLn $ "Error leyendo archivo: " ++ err) >> return (Just st)
        Right content -> do
          let lns   = filter (not . isCommentOrEmpty) (lines content)
              input = intercalate " " lns
          case parse parserComm path' input of
            Left err   -> lift (putStrLn $ "Error de parseo en " ++ path' ++ ": " ++ show err)
                          >> return (Just st)
            Right comm -> runCommInRepl st comm ("Archivo " ++ path' ++ " cargado correctamente.")

-- | Evaluación de un comando del lenguaje (la ruta principal del REPL).
--   Incluye: let, run, deposit, propose, sign, execute.
handleCommand st (EvalComm s) = do
  case parse parserComm "<interactive>" s of
    Left err   -> lift (putStrLn $ "Error de parseo: " ++ show err) >> return (Just st)
    Right comm -> runCommInRepl st comm (if hasAssign comm then "Contrato(s) asignado(s) correctamente." else "")

handleCommand st (EvalContract s) = do
  let (ist, env) = st
  case parse parserContract "<interactive>" s of
    Left err       -> lift (putStrLn $ "Error de parseo: " ++ show err) >> return (Just st)
    Right contract ->
      case substContract (isContracts ist) contract of
        Left err       -> lift (putStrLn $ ppError err) >> return (Just st)
        Right resolved -> do
          lift $ putStrLn $ "Evaluando: " ++ ppContract resolved
          case runInterp (evalContract resolved) ist env of
            Left err             -> lift (putStrLn $ ppError err) >> return (Just st)
            Right (((), _), cfs) -> do
              lift $ putStr $ ppCashflows cfs
              return (Just st)


handleCommand (ist, env) (ReplSetFecha s) =
  case parse parserTime "<interactive>" s of
    Left err -> do
      lift $ putStrLn $ "Fecha inválida: " ++ show err
      return (Just (ist, env))
    Right d  -> do
      let newIst = ist { isFechaHoy = d }
          newEnv = env { fechaHoy = d }
      lift $ putStrLn $ "Fecha actualizada a " ++ show d
      return (Just (newIst, newEnv))

handleCommand st ShowHistorial = do
  lift $ putStrLn $ ppHistorial (isHistorial (fst st))
  return (Just st)

handleCommand (ist, env) (ExportHTML s) = do
  let path = if null s then "reporte.html" else s
  msg <- lift $ exportarHTML ist env path
  lift $ putStrLn msg
  return (Just (ist, env))

handleCommand st (ExportASTSVG s) = do
  let (ist, _env) = st
      (contractStr, pathStr) = parseGraficoArg s
      defaultName = trim contractStr ++ ".svg"
      path = case pathStr of
               Just p  -> p
               Nothing -> defaultName
  case parse parserContract "<interactive>" contractStr of
    Left err -> lift $ putStrLn $ "Error de parseo: " ++ show err
    Right c  ->
      case substContract (isContracts ist) c of
        Left err -> lift $ putStrLn $ ppError err
        Right resolved -> do
          msg <- lift $ exportarASTSVG resolved path
          lift $ putStrLn msg
  return (Just st)
  where trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- ─── Helper: ejecutar un Comm y actualizar el ReplState ──────────────────────

-- | Corre un Comm via Interp, actualiza el InterpState y muestra resultados.
--   `successMsg` se muestra si no hubo cashflows (ej. "asignado correctamente").
runCommInRepl :: ReplState -> Comm -> String -> InputT IO (Maybe ReplState)
runCommInRepl (ist, env) comm successMsg = do
  case runInterp (evalComm comm) ist env of
    Left err -> do
      lift $ putStrLn $ ppError err
      return (Just (ist, env))
    Right (((), newIst), cfs) -> do
      let newEnv = env { fechaHoy = isFechaHoy newIst }
      lift $ do
        if null cfs
          then when (not (null successMsg)) $ putStrLn successMsg
          else do
            putStr $ ppCashflows cfs
            -- Si el comando fue Execute, mostrar billeteras afectadas
            showWalletDiff ist newIst cfs
      return (Just (newIst, newEnv))
-- | Muestra las billeteras de las partes involucradas en los cashflows.
showWalletDiff :: InterpState -> InterpState -> [Cashflow] -> IO ()
showWalletDiff _old new cfs = do
  let parties = uniqueParties cfs
  when (not (null parties)) $ do
    putStrLn "Billeteras actualizadas:"
    mapM_ (\p -> putStr $ ppWallet (isWallets new) p) parties

uniqueParties :: [Cashflow] -> [PartyId]
uniqueParties cfs =
  let ps = concatMap (\cf -> [desde cf, hacia cf]) cfs
  in  nubOrd ps
  where
    nubOrd []     = []
    nubOrd (x:xs) = x : nubOrd (filter (/= x) xs)

-- ─── Auxiliares ───────────────────────────────────────────────────────────────

parseGraficoArg :: String -> (String, Maybe FilePath)
parseGraficoArg s =
  case break (== '>') s of
    (contrato, [])     -> (trim contrato, Nothing)
    (contrato, _:path) -> (trim contrato, Just (trim path))
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

hasAssign :: Comm -> Bool
hasAssign (Assign _ _) = True
hasAssign (Seq c1 c2)  = hasAssign c1 || hasAssign c2
hasAssign _            = False

isCommentOrEmpty :: String -> Bool
isCommentOrEmpty s =
  let trimmed = dropWhile isSpace s
  in  null trimmed || isPrefixOf "--" trimmed

-- ─── Tabla de comandos ────────────────────────────────────────────────────────

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":ast"]     "<exp>"                   PrintAST          "Muestra el AST de un contrato"
  , Cmd [":pp"]      "<exp>"                   PrintPP           "Pretty print de un contrato/comando"
  , Cmd [":eval"]    "<exp>"                   EvalContract      "Evalúa un contrato y muestra flujos de caja"
  , Cmd [":load"]    "<archivo>"               LoadFile          "Cargar un archivo .fin"
  , Cmd [":store"]   ""                        (const ShowStore) "Mostrar contratos definidos"
  , Cmd [":quote"]   "[<nombre> <val>]"        SetQuote          "Ver/definir cotización (ej: :quote OIL 85.0)"
  , Cmd [":partes"]  "[<yo> <contra>]"         SetPartes         "Ver/cambiar partes (ej: :partes Santiago HSBC)"
  , Cmd [":wallet"]  "[<parte>]"               ShowWallet        "Ver billetera(s)"
  , Cmd [":pending"]    ""                      (const ShowPending)   "Ver contratos pendientes"
  , Cmd [":setfecha"]   "<YYYY-MM-DD>"           ReplSetFecha          "Cambiar fecha de evaluación"
  , Cmd [":historial"]  ""                      (const ShowHistorial) "Ver historial de ejecuciones"
  , Cmd [":reporte"]    "[<archivo.html>]"       ExportHTML           "Exportar reporte HTML completo"
  , Cmd [":ast-svg"]    "<contrato> > <archivo>" ExportASTSVG         "Exportar AST del contrato a SVG"
  , Cmd [":quit"]       ""                       (const Quit)         "Salir del intérprete"
  , Cmd [":help",":?"] ""                      (const Help)      "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede abreviarse a :c donde\n"
  ++ "c es el primer caracter del nombre completo.\n\n"
  ++ "Comandos del lenguaje (sin :):\n"
  ++ "  let <x> = <contrato>         definir contrato\n"
  ++ "  <contrato>                   evaluar contrato\n"
  ++ "  deposit <parte> <monto> <cur>  depositar fondos\n"
  ++ "  propose <nombre> <contrato>  proponer contrato con firma\n"
  ++ "  sign <nombre> <parte>        firmar contrato pendiente\n"
  ++ "  execute <nombre>             ejecutar contrato firmado\n"
  ++ "  if <cond> then <c> else <c>  condicional sobre observables\n"
  ++ "  setfecha YYYY-MM-DD            cambiar fecha de evaluación\n"
  ++ "  c1 ; c2                      secuenciar comandos\n\n"
  ++ "Comandos del REPL:\n"
  ++ unlines
       (map (\(Cmd c a _ d) ->
               let ct = concat (intercalate ", " (map (++ if null a then "" else " " ++ a) c) : [])
               in  ct ++ replicate (max 2 (26 - length ct)) ' ' ++ d)
            cs)