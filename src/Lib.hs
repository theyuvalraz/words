module Lib
    (grid
    , languages
    , outputGrid
    , formatGrid
    , findWord
    , findWords
    , findWordInLine
    , skew
    ) where

import Data.List (transpose, isInfixOf)
import Data

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

getLines :: Grid -> [String]
getLines grid = 
    let
        horizontal = grid
        vertical = transpose grid
        skewed = diagonalize horizontal
        tranSkewed = diagonalize (map reverse horizontal)
        lines = horizontal ++ vertical ++ skewed ++ tranSkewed
    in lines ++ (map reverse lines)

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line

findWord :: Grid -> String -> Bool
findWord grid word =
    let lines = getLines grid
    in or $ map (findWordInLine word) lines


findWords :: Grid -> [String] -> [String]
findWords grid words = filter (findWord grid) words
-- this should probably called isWordInLine
-- 1. Word we are searching for.
-- 2. The string we are searching in.
-- 3. Boolean result.
findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf


