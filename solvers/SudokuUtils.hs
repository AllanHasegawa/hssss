module SudokuUtils
(
SudokuBoard,
solveProblem
) where

import System.IO
import Data.Char

data SudokuBoard = SudokuBoard {
				rows :: Int,
				rowsInSquare :: Int,
				board :: [Maybe Int]
			} deriving (Show)

solveProblem :: Int -> (SudokuBoard -> SudokuBoard) -> IO ()
solveProblem problemIndex solver
	| problemIndex >= 0 = (do
		handle <- openFile "../problems/sudoku17.txt" ReadMode
		contents <- hGetContents handle
		print (solver $ stringToSudokuBoard $ lines contents !! problemIndex)
		print (getCols (stringToSudokuBoard $ lines contents !! problemIndex) 3)
		hClose handle
		)
	| otherwise = print "WAT?"


verifySolution :: SudokuBoard -> Bool
verifySolution b = True 

getCols' _ _ _ 0 = []
getCols' b i s n = [b !! i] ++ (getCols' b (i+s) s (n-1))

getCols :: SudokuBoard -> Int -> [[Maybe Int]]
getCols s g = let
			r = rows s
			rS = rowsInSquare s
			pInitial = ((g `mod` rS) * rS) + ((g `div` rS) * r * rS)
			steps = rows s
			b = board s
		 in [getCols' b (pInitial+n) steps rS | n <- [0..2]]


-- Transforms a string line into Sudokuboard.
-- It should be okay for inputs with numbers <9.
stringToSudokuBoard :: String -> SudokuBoard
stringToSudokuBoard str = let
				sizeGrid = floor . sqrt . fromIntegral . length $ str
				sizeRowsCols = floor . sqrt . fromIntegral $ sizeGrid
				nList = [
					if x == '0' then
						Nothing
					else
						Just $ digitToInt x
					| x <- str]
	         	  in SudokuBoard sizeGrid sizeRowsCols nList 

list1DTo2D :: Int -> [a] -> [[a]]
list1DTo2D _ [] = []
list1DTo2D n l = [take n l] ++ (list1DTo2D n $ drop n l)

defaultProblems = [10,500,4040,50340,30450]
