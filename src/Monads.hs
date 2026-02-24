module Monads where

import Types

newtype Eval a = Eval { runEval :: Env -> Either EvalError (a, [Cashflow])}

instance Functor Eval where
    fmap f (Eval h) = Eval (\env -> case h env of
                                        Left err -> Left err
                                        Right (a,cashflows) -> Right (f a, cashflows))

instance Applicative Eval where
    pure x = Eval (\_ -> Right (x,[]))
    -- Aca como <*> :: f (a -> b) -> f a -> f b entonces
    -- actionF :: Env -> Either EvalError ((a->b), [Cashflows])
    (Eval actionF) <*> (Eval g) = Eval (\env -> case actionF env of
                                            Left err -> Left err
                                            Right (f, cfs) -> case g env of
                                                                        Left e' -> Left e'
                                                                        Right (b, cfs') -> Right (f b, cfs ++ cfs'))

instance Monad Eval where
    return = pure
    (Eval h) >>= f = Eval (\env -> case h env of
                                        Left err -> Left err
                                        Right (a, cfs) -> case runEval (f a) env of
                                                            Left err' -> Left err'
                                                            Right (b, cfs') -> Right (b, cfs ++ cfs'))

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
