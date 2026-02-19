module Utils where

import AST
import Types
import Monads

ask :: Eval Env
ask = Eval (\env -> Right (env,[]))

cashflowRegister :: Cashflow -> Eval ()
cashflowRegister cf = Eval (\_ -> Right ((), [cf]))

throw :: EvalError -> Eval a
throw msg = Eval (\_ -> Left msg)

localEnv :: (Env -> Env) -> Eval a -> Eval a
localEnv f m = Eval (runEval m . f)

censor :: ([Cashflow] -> [Cashflow]) -> Eval a -> Eval a
censor g m = Eval (\env -> case runEval m env of
                            Left err -> Left err
                            Right (a, cfs) -> Right (a, g cfs))
