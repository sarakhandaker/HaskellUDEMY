data Compass = North | South | East | West
    deriving (Eq, Show, Ord, Enum)

data Expresson = Number Int
                | Add Expresson Expresson
                | Subtract Expresson Expresson
    deriving (Eq, Show, Ord)

calculate :: Expresson -> Int
calculate (Number x )= x
calculate (Add x y)= (calculate x) + (calculate y) 
calculate (Subtract x y)= (calculate x) - (calculate y )

main = print $ lookup "one" dict
    where dict = [("one", 1), ("two", 2), ("three", 3)]

    -- maybe return value will return a number (with just) or nothing
    -- use stack ghc -- -dynamic test.hs to make exe file smaller?
    -- partial function: has outputs for SOME inputs (ex. either an integer or error)


    --Partially Applied function

    -- add1 = add 1
    -- add1 b = add 1 b

    -- product vs sum types (product tyes take parameters in constructor)