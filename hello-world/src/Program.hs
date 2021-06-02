main :: IO ()
-- main = putStrLn "Hello World"


main do 
    content <- readFile "numbers.txt"
    putStrLn content


--numbers.txt has random numbers
-- can use print instead of putStrln -> can be used on any show class and prints out the string representation (with \n for example)