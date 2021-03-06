------------------------------------------------------------
--
-- Constraints0.hs
-- Code sample accompanying topic 1.3.2 "Types and type signatures"
-- See README.md for details
--
-- Fundamentals of Practical Haskell Programming
-- By Richard Cook
--
------------------------------------------------------------

myFold :: (a -> b -> b) -> b -> [a] -> b
myFold _ b [] = b
myFold f b (a : as) = myFold f (f a b) as

mySum :: Num a => [a] -> a
mySum = myFold (+) 0

-- mySum :: _ can use underscore to create a type hole
-- ghc will provide a reasonable type signature

main :: IO ()
main = print $ mySum [10, 20, 30]

-- contraints reduce the polymorphism
-- but allow more operations on the values
