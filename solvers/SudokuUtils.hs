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
		let sudokuLine = lines contents !! problemIndex
		let sudokuBoard = stringToSudokuBoard sudokuLine
		print $ solver sudokuBoard
		print $ getRowBS 7 $ sudokuBoard
		print $ getSS 7 $ sudokuBoard
		putStr $ sudokuBoardToFancyString sudokuBoard
		hClose handle
		)
	| otherwise = print "WAT?"


verifySolution :: SudokuBoard -> Bool
verifySolution b = True 

-- get row (big square)
getRowBS :: Int -> SudokuBoard -> [SudokuUnit]
getRowBS rowIndex sBoard
		| rowIndex < 0 = [Empty]
		| rowIndex >= bS sBoard = [Empty]
		| otherwise = let
				sBS = bS sBoard
				sSS = sS sBoard
				b = board sBoard
				pInitial = rowIndex * sBS
		 	in slice pInitial (pInitial+sBS) 1 b

-- get column (big square)
getColBS :: Int -> SudokuBoard -> [SudokuUnit]
getColBS colIndex sBoard 
		| colIndex < 0 = [Empty]
		| colIndex >= bS sBoard = [Empty]
		| otherwise = let
				sBS = bS sBoard
				sSS = sS sBoard
				b = board sBoard
				steps = bS sBoard
				pInitial = colIndex
		 	in slice pInitial (length $ b) steps b

-- get a small square from a big square
getSS :: Int -> SudokuBoard -> [SudokuUnit]
getSS ssIndex sBoard = let
	sBS = bS sBoard
	sSS = sS sBoard
	step = sBS
	b = board sBoard
	i = ssIndex `mod` sSS
	initial = ((ssIndex `div` sSS) * sBS * sSS) + (i * sSS)
	in subslice initial (initial + (sSS*sBS)) sBS sSS b


getSumSS :: Int -> SudokuBoard -> Maybe Int
getSumSS ssIndex sBoard 
	| ssIndex < 0 = Nothing
	| ssIndex >= sS sBoard = Nothing
	| otherwise = Nothing


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

-- Print a SudokuUnit simplified
sudokuUnitToString :: SudokuUnit -> String
sudokuUnitToString Empty = "."
sudokuUnitToString (Hint n) = show n
sudokuUnitToString (Guess n) = show n

sudokuBoardToFancyString'' :: SudokuBoard -> Int -> String
sudokuBoardToFancyString'' sBoard n
	| n `mod` (sBS*sSS) == 0 =  "\n\n"
	| n `mod` sBS == 0 = "\n"
	| n `mod` sSS == 0 = " "
	| otherwise = []
	where 	sSS = sS sBoard
		sBS = bS sBoard

sudokuBoardToFancyString' :: Int -> SudokuBoard -> [SudokuUnit] -> String
sudokuBoardToFancyString' _ _ [] = "\n"
sudokuBoardToFancyString' n sBoard sUnits@(x:xs) = 
	(sudokuBoardToFancyString'' sBoard n)
	++ (sudokuUnitToString x)
	++ " "
	++ (sudokuBoardToFancyString' (n+1) sBoard xs)

-- Creates a easy fo see string of the sudoku board
sudokuBoardToFancyString :: SudokuBoard -> String
sudokuBoardToFancyString sBoard =
	sudokuBoardToFancyString' 0 sBoard (board sBoard)

-- Aux functions
list1DTo2D :: Int -> [a] -> [[a]]
list1DTo2D _ [] = []
list1DTo2D n l = [take n l] ++ (list1DTo2D n $ drop n l)

-- slice function. Equivalent to list[from:to:step] in Python
slice :: Int -> Int -> Int -> [a] -> [a]
slice from to step list
	| from >= to = []
	| step == 1 = take (to-from) . drop from $ list
	| otherwise = [(list !! from)] ++ (slice (from+step) to step list)

-- slice function, but takes "sublists".
-- Example: subslice 1 8 2 3 "abcdefgh" == "bcdefgh"
subslice :: Int -> Int -> Int -> Int -> [a] -> [a]
subslice from to step sub list
	| sub == 1 = slice from to step list
	| from >= to = []
	| otherwise = (slice from (from+sub) 1 list)
		++ (subslice (from+step) to step sub list)

defaultProblems = [10,500,4040,50340,30450]
