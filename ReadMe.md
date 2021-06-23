How to run single HS file 

stack ghc -- Traversal.hs  
./Traversal   


Run in one step (no exe file)
stack runghc AddBrackets.hs

run ghc is an interpreter

ghci
    binds "it" to the last evaluated result 
    :type or :info to get type and definition (part of gchi not Haskell)
    :{ and :} to make a multiline expression