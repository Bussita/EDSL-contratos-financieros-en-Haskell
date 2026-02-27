module Monads where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Except

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Time (Day)
import Types
import AST

-- Estado mutable del intérprete

type ContractStore = Map String Contract
type PendingStore  = Map String PendingContract

data InterpState = InterpState
  { isContracts :: ContractStore
  , isWallets   :: Wallets
  , isPending   :: PendingStore
  , isQuotes    :: Map String Double
  , isHistorial :: [HistorialEntry]    -- registro de ejecuciones
  , isSnapshots :: [WalletSnapshot]   -- evolución temporal de billeteras
  , isFechaHoy  :: Date               -- fecha actual (mutable vía setfecha)
  } deriving (Show)

emptyInterpState :: Day -> InterpState
emptyInterpState today = InterpState
  { isContracts = Map.empty
  , isWallets   = Map.empty
  , isPending   = Map.empty
  , isQuotes    = Map.empty
  , isHistorial = []
  , isSnapshots = []
  , isFechaHoy  = today
  }

-- ─── Mónada Interp ────────────────────────────────────────────────────────────
--
-- Tipo desplegado:
--   Interp a ≅ InterpState → Env → Either EvalError ((a, InterpState), [Cashflow])

type Interp = StateT InterpState (ReaderT Env (WriterT [Cashflow] (Either EvalError)))

runInterp :: Interp a
          -> InterpState
          -> Env
          -> Either EvalError ((a, InterpState), [Cashflow])
runInterp m st env =
  runWriterT (runReaderT (runStateT m st) env)

-- ─── Primitivas ───────────────────────────────────────────────────────────────

askEnv :: Interp Env
askEnv = ask

localEnv :: (Env -> Env) -> Interp a -> Interp a
localEnv = local

getState :: Interp InterpState
getState = get

putState :: InterpState -> Interp ()
putState = put

modifyState :: (InterpState -> InterpState) -> Interp ()
modifyState = modify'

throwErr :: EvalError -> Interp a
throwErr = throwError

emitCashflow :: Cashflow -> Interp ()
emitCashflow cf = lift $ lift $ tell [cf]

censorFlows :: ([Cashflow] -> [Cashflow]) -> Interp a -> Interp a
censorFlows f m = do
  st  <- get
  env <- ask
  case runInterp m st env of
    Left err              -> throwErr err
    Right ((a, st'), cfs) -> do
      put st'
      lift $ lift $ tell (f cfs)
      return a

-- ─── Accesores ────────────────────────────────────────────────────────────────

getContracts :: Interp ContractStore
getContracts = isContracts <$> get

putContracts :: ContractStore -> Interp ()
putContracts cs = modify' (\s -> s { isContracts = cs })

getWallets :: Interp Wallets
getWallets = isWallets <$> get

putWallets :: Wallets -> Interp ()
putWallets ws = modify' (\s -> s { isWallets = ws })

getPending :: Interp PendingStore
getPending = isPending <$> get

putPending :: PendingStore -> Interp ()
putPending ps = modify' (\s -> s { isPending = ps })

getQuotes :: Interp (Map String Double)
getQuotes = isQuotes <$> get

putQuotes :: Map String Double -> Interp ()
putQuotes qs = modify' (\s -> s { isQuotes = qs })

getHistorial :: Interp [HistorialEntry]
getHistorial = isHistorial <$> get

appendHistorial :: HistorialEntry -> Interp ()
appendHistorial e = modify' (\s -> s { isHistorial = isHistorial s ++ [e] })

getSnapshots :: Interp [WalletSnapshot]
getSnapshots = isSnapshots <$> get

appendSnapshot :: WalletSnapshot -> Interp ()
appendSnapshot s = modify' (\st -> st { isSnapshots = isSnapshots st ++ [s] })

getFechaHoy :: Interp Date
getFechaHoy = isFechaHoy <$> get

putFechaHoy :: Date -> Interp ()
putFechaHoy d = modify' (\s -> s { isFechaHoy = d })