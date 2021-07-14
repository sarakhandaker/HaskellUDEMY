module Lib
    ( formatGrid
    , outputGrid
    , skew
    , findWord
    , findWords
    , findWordInLine
    , gridWithCoords
    , zipOverGrid
    , zipOverGridWith
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid a = [[a]]
data Cell = Cell (Integer, Integer) Char | Empty deriving (Eq, Ord, Show)

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
    let cols= repeat [0..]
        rows = map repeat [0..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char ->  Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Char -> IO ()
outputGrid grid = putStrLn $ formatGrid grid

formatGrid :: Grid Char -> String
formatGrid = unlines

getLines :: Grid Char -> [String]
getLines grid = 
    let horizontal = grid 
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)

diagonalize :: Grid Char -> Grid Char
diagonalize = transpose . skew

skew :: Grid Char -> Grid Char
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line

findWord :: Grid Char -> String -> Maybe String
findWord grid word = 
    let lines = getLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

findWords :: Grid Char -> [String] -> [String]
findWords grid words = 
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf