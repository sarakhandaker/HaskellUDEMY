import Data.Complex
import Data.Ratio

type Port = Int
type HostInfo = (String, Int)

-- NUM TYPES
n0 :: Int
n0 = 5

n1 :: Double
n1 = 5.0

n2 :: Complex Double
n2 = 2 :+ 3

-- this evaluated to 2 + 3i (imaginary numbers)

n3 :: Ratio Int
n3 = 2 % 3

-- CHAR TYPES
c0 :: Char
c0 = 'X'

-- the following are unicode literals

c1 :: Char
c1 = '\0088'

c2 :: Char
c2 = '\x0058'

c3 :: Char
c3 = '\o0130'

--STRING TYPES

s0 :: String
s0 = "abc"

s1 :: String
s1 = "\0088\x0058\o0130"


--LIST TYPES
l0 :: [Int]
l0 = [1, 2, 3, 4, 5]

l1 :: [Int]
l1 = [1..10]

l2 :: [Int]
l2 = [1, 3..10]

-- infinite list
l3 :: [Int]
l3 = [1..]

l4 :: [String]
l4 = ["aaa", "bbb", "ccc", "ddd"]

-- both are a list of chars
l5 :: [Char]
l5 = ['a', 'b', 'c', 'd']

l6 :: [Char]
l6 = "abcd"

-- TUPLES

t0 :: (Int, Int)
t0 = (1234, 5678)

t1 :: (String, Int, Double)
t1 = ("sometext", 8, 3.141)

t2 :: ([Int], [String], (Float, Char))
t2 = ([1, 2, 3, 4], ["aaa", "bbb"], (1.0, 'o'))

main :: IO ()
main = do
    print n0