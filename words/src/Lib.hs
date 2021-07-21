module Lib
    ( formatGrid
    , outputGrid
    , skew
    , findWord
    , findWords
    , findWordInCellInfix
    , findWordInCellPrefix
    , gridWithCoords
    , zipOverGrid
    , cell2char
    , Game (Game, gameGrid, gameWords)
    , Cell (Cell, Indent)
    , formatGame
    , makeGame
    , totalWords
    , score
    , playWord
    , zipOverGridWith
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M

data Game = Game { gameGrid  :: Grid Cell
                 , gameWords :: M.Map String (Maybe [Cell])
                 } deriving (Show)
data Cell = Cell (Integer, Integer) Char | Indent deriving (Eq, Ord, Show)
type Grid a = [[a]]

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let grid'  = gridWithCoords grid
      words' = M.fromList $ map (\word -> (word, Nothing)) words
  in Game grid' words'

totalWords :: Game -> Int
totalWords game = length $ M.keys (gameWords game)

score :: Game -> Int
score game = length $ catMaybes $ M.elems (gameWords game)

playWord :: Game -> String -> Game
playWord game word | not (M.member word (gameWords game)) = game
playWord game word =
  let grid = gameGrid game
      foundWord = findWord grid word
      newGame = case foundWord of
        Nothing -> game
        Just cs ->
          let words = gameWords game
              words' = M.insert word foundWord words
          in Game grid words'
  in newGame

playGame game = do
  let grid = gameGrid game
      words = gameWords game
      s = score game
      t = totalWords game
  hSetBuffering stdout NoBuffering
  putStrLn ((show s) ++ "/" ++ (show t))
  if s < t then do
    putStr (formatGrid grid)
    putStr "Enter a word> "
    word <- getLine
    let newGame = playWord game word
    playGame newGame
  else
    putStrLn "Congratulations!"

formatGame :: Game -> String
formatGame game = formatGrid grid 
                ++ "\n\n"
                ++ (show $ score game)
                ++ "/"
                ++ (show $ totalWords game)

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

-- use map . map over a nested set of lists
mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
    let cols= repeat [0..]
        rows = map repeat [0..]
    in zipOverGrid rows cols

gridWithCoords :: Grid Char ->  Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn $ formatGrid grid

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

getLines :: Grid Cell -> [[Cell]]
getLines grid = 
    let horizontal = grid 
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse grid)
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      foundWords = map (findWordInCellInfix word) lines
  in listToMaybe (catMaybes foundWords)

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words = 
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

-- findWordInLine :: String -> [Cell] -> Maybe [Cell]
-- findWordInLine = undefined -- isInfixOf

findWordInCellInfix :: String -> [Cell] -> Maybe [Cell]
findWordInCellInfix _ [] = Nothing
findWordInCellInfix word line =
  let foundWord = findWordInCellPrefix [] word line
  in case foundWord of
       Nothing -> findWordInCellInfix word (tail line)
       Just _ -> foundWord

findWordInCellPrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellPrefix acc (x:xs) (c:cs) | x == cell2char c
                                  = findWordInCellPrefix (c : acc) xs cs
findWordInCellPrefix acc []     _ = Just (reverse acc)
findWordInCellPrefix _    _     _ = Nothing