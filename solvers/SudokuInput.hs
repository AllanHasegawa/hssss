module SudokuInput
(
solveProblem
) where

import System.IO

type SudokuBoard = [[Int]]

solveProblem :: Int -> (SudokuBoard -> SudokuBoard) -> IO ()
solveProblem problemIndex solver
	| problemIndex >= 0 = (do
		handle <- openFile "../problems/sudoku17.txt" ReadMode
		contents <- hGetContents handle
		print (lines contents !! problemIndex)
		hClose handle
		)
	| otherwise = print "WAT?"


stringToSudokuBoard :: String -> SudokuBoard
stringToSudokuBoard str = [[10]]

defaultProblems = [10,500,4040,50340,30450]
