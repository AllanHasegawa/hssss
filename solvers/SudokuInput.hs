module SudokuInput
(
SudokuBoard,
solveProblem
) where

import System.IO
import Data.Char

type SudokuBoard = [[[Maybe Int]]]

solveProblem :: Int -> (SudokuBoard -> SudokuBoard) -> IO ()
solveProblem problemIndex solver
	| problemIndex >= 0 = (do
		handle <- openFile "../problems/sudoku17.txt" ReadMode
		contents <- hGetContents handle
		print (stringToSudokuBoard $ lines contents !! problemIndex)
		hClose handle
		)
	| otherwise = print "WAT?"


-- Transforms a string line into Sudokuboard.
-- It should be okay for inputs with numbers <9.
stringToSudokuBoard :: String -> SudokuBoard
stringToSudokuBoard str =	let 	mList =
						[if x == '0' then
							Nothing else
							Just $ digitToInt x
						| x <- str]
					sizeEachInnerCube = floor . sqrt . fromIntegral . length $ str
					sizeEachRowCol = floor . sqrt . fromIntegral $ sizeEachInnerCube
					list2D = list1DTo2D sizeEachInnerCube mList
					list3D = [list1DTo2D sizeEachRowCol x | x <- list2D]
				in list3D


list1DTo2D :: Int -> [a] -> [[a]]
list1DTo2D _ [] = []
list1DTo2D n l = [take n l] ++ (list1DTo2D n $ drop n l)

defaultProblems = [10,500,4040,50340,30450]
