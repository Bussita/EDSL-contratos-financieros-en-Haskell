module Evaluator where

import qualified Data.Map as Map
import Data.Map (Map)

import Types
import AST
import Monads
import Utils

type ContractStore = Map String Contract

substContract :: ContractStore -> Contract -> Either EvalError Contract
substContract _     Zero          = Right Zero
substContract _     (One c)       = Right (One c)
substContract store (Give c)      = Give <$> substContract store c
substContract store (And c1 c2)   = And <$> substContract store c1 <*> substContract store c2
substContract store (Or c1 c2)    = Or <$> substContract store c1 <*> substContract store c2
substContract store (Then c1 c2)  = Then <$> substContract store c1 <*> substContract store c2
substContract store (Truncate d c)= Truncate d <$> substContract store c
substContract store (Scale o c)   = Scale o <$> substContract store c
substContract store (Get c)       = Get <$> substContract store c
substContract store (Anytime c)   = Anytime <$> substContract store c
substContract store (Var name)    = case Map.lookup name store of
    Just c  -> substContract store c
    Nothing -> Left (EvalMsg $ "Contrato no definido: " ++ name)

evalComm :: ContractStore -> Env -> Comm -> Either EvalError (ContractStore, [Cashflow])
evalComm store env (Assign name contract) =
    case substContract store contract of
        Left err -> Left err
        Right resolved -> Right (Map.insert name resolved store, [])

evalComm store env (Run contract) =
    case substContract store contract of
        Left err -> Left err
        Right resolved -> case runEval (evalContract resolved) env of
            Left err          -> Left err
            Right ((), cfs)   -> Right (store, cfs)

evalComm store env (Seq c1 c2) = do
    (store1, cfs1) <- evalComm store env c1
    (store2, cfs2) <- evalComm store1 env c2
    Right (store2, cfs1 ++ cfs2)

-- Observables
evalObs :: Obs Double -> Eval Double
evalObs (Konst x)    = return x
evalObs (External s) = do
    env <- ask
    return (getQuote env s)
evalObs (Add a b) = (+) <$> evalObs a <*> evalObs b
evalObs (Sub a b) = (-) <$> evalObs a <*> evalObs b
evalObs (Mul a b) = (*) <$> evalObs a <*> evalObs b
evalObs (Div a b) = do
    va <- evalObs a
    vb <- evalObs b
    if vb == 0 then throw DivByZero
               else return (va / vb)
evalObs (Neg a) = negate <$> evalObs a


evalContract :: Contract -> Eval ()
evalContract Zero = return ()

evalContract (One c) = do
    env <- ask
    let amt = Amount 1.00 c
    let cf  = Cashflow (fechaHoy env) amt (contraparte env) (yo env)
    cashflowRegister cf

evalContract (Give c) = do
    let g env = env { yo = contraparte env, contraparte = yo env }
    localEnv g (evalContract c)

evalContract (And c1 c2) = do
    evalContract c1
    evalContract c2

--Or c1 c2: elegir el contrato más favorable.
--   Heurística: se evalúan ambos y se elige el que genera mayor valor neto para "yo".
evalContract (Or c1 c2) = do
    env <- ask
    let r1 = runEval (evalContract c1) env
    let r2 = runEval (evalContract c2) env
    case (r1, r2) of
        (Left _, Left _)               -> throw (EvalMsg "Ambas ramas de 'or' fallaron")
        (Left _, Right _)              -> evalContract c2
        (Right _, Left _)              -> evalContract c1
        (Right ((), cfs1), Right ((), cfs2)) ->
            if netValue (yo env) cfs1 >= netValue (yo env) cfs2
                then evalContract c1
                else evalContract c2

evalContract (Then c1 c2) = do
    env <- ask
    case runEval (evalContract c1) env of
        Left _           -> evalContract c2
        Right ((), cfs)  ->
            if null cfs
                then evalContract c2
                else mapM_ cashflowRegister cfs

evalContract (Scale o c) = do
    factor <- evalObs o
    let scaleFlow cf = cf { cantidad = (cantidad cf) { value = value (cantidad cf) * factor } }
    censor (map scaleFlow) (evalContract c)

evalContract (Truncate limitDate c) = do
    let g = filter (\x -> fecha x <= limitDate)
    censor g (evalContract c)

-- Preguntar si hace falta.
evalContract (Get c) = evalContract c

evalContract (Anytime c) = evalContract c

-- no debería llegar acá (substContract lo resuelve antes).
evalContract (Var name) = throw (EvalMsg $ "Variable no resuelta: " ++ name)


-- | Calcular el valor neto de una lista de cashflows para una parte.
--   Positivo = recibe, Negativo = paga.
netValue :: PartyId -> [Cashflow] -> Double
netValue party = sum . map cf
  where
    cf flow
        | hacia flow == party = value (cantidad flow)
        | desde flow == party = negate (value (cantidad flow))
        | otherwise           = 0
