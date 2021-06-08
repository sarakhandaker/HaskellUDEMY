-- lamba functions (anonymous)

\x -> x+1

parenthesizeWords s = unwords $ map parenthesizeWord (words s)
    where parenthesizeWord s = "(" ++ s ++ ")"

    -- words is in prelude (take a string and separates on white space)
    -- unwords is the join (opposite to words)

    --using lamdas
parenthesizeWords' s = unwords $ map (\s -> "(" ++ s ++ ")") (words s)

filter (\x -> x< 5) [1..10]

-- partial functions 
filter (< 5) [1..10]