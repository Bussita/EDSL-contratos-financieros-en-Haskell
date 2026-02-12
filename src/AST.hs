import Types

data Contract =
    Zero |
    One Currency |
    Give Contract |
    And Contract Contract |
    Or Contract Contract |
    Truncate Date Contract |
    Then Contract Contract |
    Scale (Obs Double) Contract |
    Get Contract |
    Anytime Contract 
    deriving Show

data Obs a =
    Konst a |
    Lift1 (a -> a) (Obs a) |
    Lift2 (a -> a -> a) (Obs a) (Obs a) |
    External String 
    deriving Show



instance Num a => Num (Observable a) where
    (+)         = Lift2 (+)
    (*)         = Lift2 (*)
    (-)         = Lift2 (-)
    abs         = Lift1 abs
    signum      = Lift1 signum
    fromInteger = Konst . fromInteger