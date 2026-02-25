module Evaluator where

import           Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types
import AST
import Monads

-- Sustitución de variables

-- Expande todas las variables (Var) de un contrato usando el store,
-- detectando referencias cíclicas.
substContract :: ContractStore -> Contract -> Either EvalError Contract
substContract store = go Set.empty
  where
    go _       Zero            = Right Zero
    go _       (One c)         = Right (One c)
    go visited (Give c)        = Give      <$> go visited c
    go visited (And c1 c2)     = And       <$> go visited c1 <*> go visited c2
    go visited (Or  c1 c2)     = Or        <$> go visited c1 <*> go visited c2
    go visited (Then c1 c2)    = Then      <$> go visited c1 <*> go visited c2
    go visited (Truncate d c)  = Truncate d <$> go visited c
    go visited (Scale o c)     = Scale o   <$> go visited c
    go visited (Get c)         = Get       <$> go visited c
    go visited (Anytime c)     = Anytime   <$> go visited c
    go visited (Var name)
      | Set.member name visited =
          Left (EvalMsg $ "Referencia cíclica detectada en variable: " ++ name)
      | otherwise =
          case Map.lookup name store of
            Just c  -> go (Set.insert name visited) c
            Nothing -> Left (EvalMsg $ "Contrato no definido: " ++ name)

-- Evaluador de comandos

-- Evalúa un comando actualizando el InterpState dentro de Interp.
--   Los cashflows se acumulan en el Writer.
evalComm :: Comm -> Interp ()

evalComm (Assign name contract) = do
  cs <- getContracts
  putContracts (Map.insert name contract cs)

evalComm (Seq c1 c2) = do
  evalComm c1
  evalComm c2

evalComm (Run contract) = do
  cs <- getContracts
  case substContract cs contract of
    Left err       -> throwErr err
    Right resolved -> evalContract resolved

-- Evaluador de comandos referidos a una operación de billetera

evalComm (Deposit party cur amount) = do
  ws <- getWallets
  putWallets (walletDeposit party cur amount ws)

evalComm (Propose name contract) = do
  ps  <- getPending
  env <- askEnv
  let today = fechaHoy env
      me    = yo env
      them  = contraparte env
      pc    = createPending name contract me them today
  putPending (Map.insert name pc ps)

evalComm (Sign name party) = do
  ps <- getPending
  case Map.lookup name ps of
    Nothing -> throwErr (EvalMsg $ "No existe contrato pendiente '" ++ name ++ "'.")
    Just pc ->
      case signContract party pc of
        Left err  -> throwErr (EvalMsg err)
        Right pc' -> putPending (Map.insert name pc' ps)

evalComm (Execute name) = do
  ps <- getPending
  case Map.lookup name ps of
    Nothing -> throwErr (EvalMsg $ "No existe contrato pendiente '" ++ name ++ "'.")
    Just pc -> do
      if not (isFullySigned pc)
        then throwErr (EvalMsg $ "Contrato '" ++ name ++ "' no está completamente firmado.")
        else do
          cs <- getContracts
          case substContract cs (pcContract pc) of
            Left err       -> throwErr err
            Right resolved -> do
              let adjustEnv e = e { yo = pcPartyA pc, contraparte = pcPartyB pc }
              ws  <- getWallets
              env <- askEnv
              -- Ejecutamos el contrato en modo captura: obtenemos los CFs sin
              -- emitirlos todavía al Writer global, para poder verificar saldos.
              st <- getState
              case runInterp (localEnv adjustEnv (evalContract resolved)) st env of
                Left err -> throwErr err
                Right (((), _), cfs) ->
                  case applyWalletCashflows ws cfs of
                    Left err         -> throwErr (EvalMsg err)
                    Right newWallets -> do
                      -- Todo OK: emitir CFs al Writer global y actualizar estado
                      mapM_ emitCashflow cfs
                      putWallets newWallets
                      putPending (Map.delete name ps)

-- Evaluador de observables (en la práctica solo se usa el tipo Obs Double)

evalObs :: Obs Double -> Interp Double

evalObs (Konst x)    = return x

evalObs (External s) = do
  env <- askEnv
  case getQuote env s of
    Just v  -> return v
    Nothing -> throwErr (UnknownObs s)

evalObs (Balance party cur) = do
  ws <- getWallets
  return (walletBalance ws party cur)

evalObs (Add a b) = (+) <$> evalObs a <*> evalObs b
evalObs (Sub a b) = (-) <$> evalObs a <*> evalObs b
evalObs (Mul a b) = (*) <$> evalObs a <*> evalObs b
evalObs (Neg a)   = negate <$> evalObs a
evalObs (Div a b) = do
  va <- evalObs a
  vb <- evalObs b
  if vb == 0 then throwErr DivByZero else return (va / vb)

-- Evaluador de contratos

evalContract :: Contract -> Interp ()

evalContract Zero = return ()

evalContract (One c) = do
  env <- askEnv
  let amt = Amount 1.00 c
      cf  = Cashflow (fechaHoy env) amt (contraparte env) (yo env)
  emitCashflow cf

evalContract (Give c) =
  localEnv (\e -> e { yo = contraparte e, contraparte = yo e })
           (evalContract c)

evalContract (And c1 c2) = evalContract c1 >> evalContract c2

evalContract (Or c1 c2) = do
  st  <- getState
  env <- askEnv
  let r1 = runInterp (evalContract c1) st env
      r2 = runInterp (evalContract c2) st env
  case (r1, r2) of
    (Left _, Left _)   -> throwErr (EvalMsg "Ambas ramas de 'or' fallaron")
    (Left _, Right _)  -> evalContract c2
    (Right _, Left _)  -> evalContract c1
    (Right ((_,_), cfs1), Right ((_,_), cfs2)) -> do
      let oracle = getQuote env
      case (netValue oracle (yo env) cfs1, netValue oracle (yo env) cfs2) of
        (Right v1, Right v2) ->
          if v1 >= v2 then evalContract c1 else evalContract c2
        (Left e, _) -> throwErr e
        (_, Left e) -> throwErr e

evalContract (Then c1 c2) = do
  st  <- getState
  env <- askEnv
  case runInterp (evalContract c1) st env of
    Left _                      -> evalContract c2
    Right (((), _), cfs)
      | null cfs                -> evalContract c2
      | otherwise               -> mapM_ emitCashflow cfs

evalContract (Scale o c) = do
  factor <- evalObs o
  let scaleFlow cf = cf { cantidad = (cantidad cf) { value = value (cantidad cf) * factor } }
  censorFlows (map scaleFlow) (evalContract c)

evalContract (Truncate limitDate c) =
  censorFlows (filter (\x -> fecha x <= limitDate)) (evalContract c)

evalContract (Get c)     = evalContract c
evalContract (Anytime c) = evalContract c

evalContract (Var name) =
  throwErr (EvalMsg $ "Variable no resuelta: " ++ name)

-- Operaciones en las billeteras

walletBalance :: Wallets -> PartyId -> Currency -> Double
walletBalance ws party cur =
  case Map.lookup party ws of
    Nothing  -> 0
    Just bal -> Map.findWithDefault 0 cur bal

walletDeposit :: PartyId -> Currency -> Double -> Wallets -> Wallets
walletDeposit party cur amount ws =
  let current  = walletBalance ws party cur
      partyMap = Map.findWithDefault Map.empty party ws
  in  Map.insert party (Map.insert cur (current + amount) partyMap) ws

walletWithdraw :: PartyId -> Currency -> Double -> Wallets
               -> Either String Wallets
walletWithdraw party cur amount ws =
  let current = walletBalance ws party cur
  in  if current < amount
        then Left $ party ++ " no tiene saldo suficiente en " ++ show cur
                  ++ " (tiene " ++ show current
                  ++ ", necesita " ++ show amount ++ ")"
        else Right $ walletDeposit party cur (negate amount) ws

applyWalletCashflows :: Wallets -> [Cashflow] -> Either String Wallets
applyWalletCashflows ws cfs = foldM applyOne ws cfs
  where
    applyOne acc cf =
      let rawAmt = value (cantidad cf)
          (payer, receiver, amt)
            | rawAmt >= 0 = (desde cf, hacia cf, rawAmt)
            | otherwise   = (hacia cf, desde cf, abs rawAmt)
          cur = currency (cantidad cf)
      in  case walletWithdraw payer cur amt acc of
            Left err   -> Left err
            Right acc' -> Right (walletDeposit receiver cur amt acc')

-- Auxiliares para obtener valor neto de los cashflows

toBaseValue :: (String -> Maybe Double) -> Amount -> Either EvalError Double
toBaseValue oracle (Amount v cur) =
  case cur of
    USD -> Right v
    _   -> case oracle (show cur ++ "_USD") of
             Just rate -> Right (v * rate)
             Nothing   -> Left (EvalMsg $ "No hay cotización para " ++ show cur)

netValue :: (String -> Maybe Double) -> PartyId -> [Cashflow] -> Either EvalError Double
netValue oracle party cfs = fmap sum (mapM cf cfs)
  where
    cf flow
      | hacia flow == party = toBaseValue oracle (cantidad flow)
      | desde flow == party = fmap negate (toBaseValue oracle (cantidad flow))
      | otherwise           = Right 0