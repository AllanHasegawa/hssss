module SudokuUtils
(
SudokuBoard,
solveProblem
) where

import System.IO
import Data.Char

data SudokuUnit = Empty | Hint Int | Guess Int deriving (Show)

-- sS :: size of the small square
-- bS :: size of the big square
-- board :: list of SudokuUnit's
data SudokuBoard = SudokuBoard {
				sS :: Int,
				bS :: Int,
				board :: [SudokuUnit]
			} deriving (Show)

solveProblem :: Int -> (SudokuBoard -> SudokuBoard) -> IO ()
solveProblem problemIndex solver
	| problemIndex >= 0 = (do
		handle <- openFile "../problems/sudoku17.txt" ReadMode
		contents <- hGetContents handle
		print (solver $ stringToSudokuBoard $ lines contents !! problemIndex)
		print (getRowBS (stringToSudokuBoard $ lines contents !! problemIndex) 6)
		hClose handle
		)
	| otherwise = print "WAT?"


verifySolution :: SudokuBoard -> Bool
verifySolution b = True 

-- get row (big square)
getRowBS sBoard rowIndex
		| rowIndex < 0 = [Empty]
		| rowIndex >= bS sBoard = [Empty]
		| otherwise = let
				sBS = bS sBoard
				sSS = sS sBoard
				b = board sBoard
				pInitial = rowIndex * sBS
		 	in slice pInitial (pInitial+sBS) 1 b

-- get column (big square)
getColBS :: SudokuBoard -> Int -> [SudokuUnit]
getColBS sBoard colIndex
		| colIndex < 0 = [Empty]
		| colIndex >= bS sBoard = [Empty]
		| otherwise = let
				sBS = bS sBoard
				sSS = sS sBoard
				b = board sBoard
				steps = bS sBoard
				pInitial = colIndex
		 	in slice pInitial (length $ b) steps b


-- Transforms a string line into Sudokuboard.
-- It should be okay for inputs with numbers <9.
stringToSudokuBoard :: String -> SudokuBoard
stringToSudokuBoard str = let
				sizeBS = floor . sqrt . fromIntegral . length $ str
				sizeSS = floor . sqrt . fromIntegral $ sizeBS
				nList = [
					if x == '0' then
						Empty
					else
						Hint $ digitToInt x
					| x <- str]
	         	  in SudokuBoard sizeSS sizeBS nList 

-- Aux functions
list1DTo2D :: Int -> [a] -> [[a]]
list1DTo2D _ [] = []
list1DTo2D n l = [take n l] ++ (list1DTo2D n $ drop n l)

slice :: Int -> Int -> Int -> [a] -> [a]
slice from to step list
	| from >= to = []
	| otherwise = [(list !! from)] ++ (slice (from+step) to step list)

defaultProblems = [10,500,4040,50340,30450]
