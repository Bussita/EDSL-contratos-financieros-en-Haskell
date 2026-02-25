module Monads where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Except

import qualified Data.Map as Map
import Data.Map (Map)

import Types
import AST


type ContractStore = Map String Contract
type PendingStore  = Map String PendingContract

data InterpState = InterpState
  { isContracts :: ContractStore     -- variables: "let x = ..."
  , isWallets   :: Wallets           -- saldos de las partes
  , isPending   :: PendingStore      -- contratos pendientes de firma
  , isQuotes    :: Map String Double -- cotizaciones (actualizable vía setQuote)
  } deriving (Show)

emptyInterpState :: InterpState
emptyInterpState = InterpState
  { isContracts = Map.empty
  , isWallets   = Map.empty
  , isPending   = Map.empty
  , isQuotes    = Map.empty
  }

-- ─── Mónada Interp ────────────────────────────────────────────────────────────
--
-- Apila cuatro efectos:
--
--   ExceptT EvalError   →  fallo con error tipado (reemplaza Either)
--   WriterT [Cashflow]  →  acumula flujos de caja (Writer puro)
--   ReaderT Env         →  entorno inmutable (fecha, partes, oráculo)
--   StateT  InterpState →  estado mutable (wallets, contratos, pending)
--
-- El orden de los transformadores importa.  Ponemos StateT afuera de
-- ExceptT para que un fallo no deshaga el estado (semántica "fallo aislado").
-- Si prefirieras rollback automático al fallar, invert State y Except.
--
-- Tipo desplegado:
--   Interp a ≅ InterpState
--            → Env
--            → Either EvalError ((a, InterpState), [Cashflow])

type Interp = StateT InterpState (ReaderT Env (WriterT [Cashflow] (Either EvalError)))

-- Ejecuta la mónada dado un estado inicial y un entorno.
runInterp :: Interp a
          -> InterpState
          -> Env
          -> Either EvalError ((a, InterpState), [Cashflow])
runInterp m st env =
  runWriterT (runReaderT (runStateT m st) env)

-- Primitivas de la mónada

-- Lee el entorno inmutable.
askEnv :: Interp Env
askEnv = ask

-- Ejecuta una acción con un entorno modificado localmente.
localEnv :: (Env -> Env) -> Interp a -> Interp a
localEnv = local

-- Lee el estado mutable completo.
getState :: Interp InterpState
getState = get

-- Reemplaza el estado mutable completo.
putState :: InterpState -> Interp ()
putState = put

-- Modifica el estado mutable.
modifyState :: (InterpState -> InterpState) -> Interp ()
modifyState = modify'

-- Lanza un error de evaluación.
throwErr :: EvalError -> Interp a
throwErr = throwError

-- Registra un cashflow en el Writer.
emitCashflow :: Cashflow -> Interp ()
emitCashflow cf = lift $ lift $ tell [cf]

-- Transforma los cashflows acumulados hasta ahora dentro de una
--   sub-computación.
censorFlows :: ([Cashflow] -> [Cashflow]) -> Interp a -> Interp a
censorFlows f m = do
  st  <- get
  env <- ask
  -- Corremos m en el mismo estado/env pero capturando sus cashflows por separado
  case runInterp m st env of
    Left err               -> throwErr err
    Right ((a, st'), cfs)  -> do
      put st'
      lift $ lift $ tell (f cfs)
      return a

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