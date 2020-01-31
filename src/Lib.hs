-- Exporting functions to be used in the Main file.
module Lib
    (grid
    , languages
    , outputGrid
    , formatGrid
    , findWord
    , findWords
    , findWordInLine
    , skew
    , squareMatrixCoords
    , og
    , coordGrid
    ) where

-- Importing needed package.
import Data.List (transpose, isInfixOf)
import Data

--Defining generic array of arrays as "Grid"
type Grid a = [[a]]
-- Defining Cell as a tuple of Ints.
data Cell = Cell (Int, Int) Char deriving (Eq, Ord, Show)

-- Prints the grid after formating.
outputGrid :: Grid Char -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

-- Concatenates the grid and adds new line at the end of every array for visibility.
-- Input "Grid" , output String.
formatGrid :: Grid Char -> String
formatGrid = unlines

-- Takes the grid and reads it from all directions.
-- Concatenates all variations to original grid.
-- Input "Grid" type output Array of String.
getLines :: Grid Char -> [String]
getLines grid = 
    let
        horizontal = grid
        vertical = transpose grid
        skewed = diagonalize horizontal
        tranSkewed = diagonalize (map reverse horizontal)
        lines = horizontal ++ vertical ++ skewed ++ tranSkewed
    in lines ++ (map reverse lines)

-- transposes the skewed grid.
-- Inputs and outputs the Grid type.
diagonalize :: Grid Char -> Grid Char
diagonalize = transpose . skew

-- Recursive function that adds underscore characters to every line, 
-- Next line gets more underscores.
-- Inputs and outputs the Grid type.
skew :: Grid Char -> Grid Char
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line

-- Returns true if the Word String is contained tin the Grid.
-- Arguments are "Grid" type, String, returns a boolean value.
findWord :: Grid Char -> String -> Bool
findWord grid word =
    let lines = getLines grid
    in or $ map (findWordInLine word) lines

-- Using the findWord function to filter the existing words in the grid.
-- Arguments are "Grid" type, array of String, returns an array of String.
findWords :: Grid Char -> [String] -> [String]
findWords grid words = filter (findWord grid) words

-- This should probably be called isWordInLine
-- 1. Word we are searching for.
-- 2. The string we are searching in.
-- 3. Boolean result.
findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

squareMatrixCoords:: (Num a, Enum a) => a -> a -> Grid (a, a)
squareMatrixCoords columns rows = map (
    (\a b -> zip (repeat b) [0 .. a]) columns
    ) [0 .. rows]

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordGrid :: Grid Cell
coordGrid = zipOverGridWith Cell (squareMatrixCoords (length (head grid)) (length  grid)) grid

og :: Show a => [a] -> IO()
og = putStrLn . unlines . map show

