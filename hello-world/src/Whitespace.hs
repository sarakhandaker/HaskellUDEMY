main = 
    let 
        x = 5
        y = 7
    in print (y + x)

    -- uses let binding to introduce the names into the scope print expression
    -- x and y must line up (group expressions)
    -- where is similar to a let binding (name introduced after the binding instead of before)

f = do 
        a
        b
        c
g = do a
       b
       c

a :: IO ()
a = undefined; b = undefined; c = undefined
-- use semicolon to write group expressions on the same line

--BRACES

main2 = let {x = 5; y = 7} in print (y + x)