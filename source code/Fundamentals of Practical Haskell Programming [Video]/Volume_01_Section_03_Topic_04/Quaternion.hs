------------------------------------------------------------
--
-- Quaternion1.hs
-- Code sample accompanying topic 1.3.4 "Type classes"
-- See README.md for details
--
-- Fundamentals of Practical Haskell Programming
-- By Richard Cook
--
------------------------------------------------------------

data Quaternion = Q
    { qR :: Double
    , qI :: Double
    , qJ :: Double
    , qK :: Double
    }

-- without this, Quaternion doesnt have a show instance
instance Show Quaternion where
    show q = "(" ++
        show (qR q) ++ " + " ++
        show (qI q) ++ "i + " ++
        show (qJ q) ++ "j + " ++
        show (qK q) ++ "k)"

-- this is so common in Haskell you can just do the following:
-- data Quaternion = Q
--     { qR :: Double
--     , qI :: Double
--     , qJ :: Double
--     , qK :: Double
--     } deriving Show

instance Num Quaternion where
    q0 + q1 = Q (qR q0 + qR q1) (qI q0 + qI q1) (qJ q0 + qJ q1) (qK q0 + qK q1)
    q0 * q1 = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined

main :: IO ()
main = print $ Q 1 2 3 4
        print $ Q 10 20 30 40 + Q 100 200 300 400
