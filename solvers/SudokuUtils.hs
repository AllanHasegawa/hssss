module SudokuUtils
(
SudokuBoard,
solveProblem
) where

import System.IO
import Data.Char
import Data.List

data SudokuUnit = Empty | Hint Int | Guess Int deriving (Show,Read,Eq)

-- sS :: size of the small square
-- bS :: size of the big square
-- board :: list of SudokuUnit's
data SudokuBoard = SudokuBoard {
				sS :: Int,
				bS :: Int,
				board :: [SudokuUnit]
			} deriving (Show,Read)


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
		print $ verifySolutionRows sudokuBoard
		print $ getRowBS 0 sudokuBoard
		hClose handle
		)
	| otherwise = print "WAT?"

-- verify if solution is correct
-- True solution is correct
-- False solution is wrong
verifySolution :: SudokuBoard -> Bool
verifySolution b = and $ sequence
	[verifySolutionRows,verifySolutionCols,verifySolutionSS] b

verifySolutionSS :: SudokuBoard -> Bool
verifySolutionSS sBoard = let
	sBS = bS sBoard
	sSs = getSSs sBoard
	filtered = filter (\x -> (length $ uniqueSudokuUnits x) == sBS) sSs
	in (length filtered) > 0

verifySolutionRows :: SudokuBoard -> Bool
verifySolutionRows sBoard = let
	sBS = bS sBoard
	rows = getRowsBS sBoard
	filtered = filter (\x -> (length $ uniqueSudokuUnits x) == sBS) rows
	in (length filtered) > 0
	
verifySolutionCols :: SudokuBoard -> Bool
verifySolutionCols sBoard = let
	sBS = bS sBoard
	cols = getColsBS sBoard
	filtered = filter (\x -> (length $ uniqueSudokuUnits x) == sBS) cols
	in (length filtered) > 0

-- get ALL small squares from big square
getSSs :: SudokuBoard -> [[SudokuUnit]]
getSSs sBoard = let
	sBS = bS sBoard
	in map (flip getSS sBoard) [0..(sBS-1)]

-- get ALL cols from big square
getColsBS :: SudokuBoard -> [[SudokuUnit]]
getColsBS sBoard = let
	sBS = bS sBoard
	in map (flip getColBS sBoard) [0..(sBS-1)]

-- get ALL rows from big square
getRowsBS :: SudokuBoard -> [[SudokuUnit]]
getRowsBS sBoard = let
	sBS = bS sBoard
	in map (flip getRowBS sBoard) [0..(sBS-1)]

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

-- Returns uniques values (without Empty)
uniqueSudokuUnits :: [SudokuUnit] -> [SudokuUnit]
uniqueSudokuUnits sUnits = filter (\x -> x /= Empty) $  nub sUnits

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

getPureProblem :: SudokuBoard
--getPureProblem = read "SudokuBoard {sS = 3, bS = 9, board = [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 1,Empty,Hint 4,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 2,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 5,Empty,Hint 4,Empty,Hint 7,Empty,Empty,Hint 8,Empty,Empty,Empty,Hint 3,Empty,Empty,Empty,Empty,Hint 1,Empty,Hint 9,Empty,Empty,Empty,Empty,Hint 3,Empty,Empty,Hint 4,Empty,Empty,Hint 2,Empty,Empty,Empty,Hint 5,Empty,Hint 1,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 8,Empty,Hint 6,Empty,Empty,Empty]}"
getPureProblem = read "SudokuBoard {sS = 3, bS = 9, board = [Hint 2,Hint 3,Hint 4,Hint 5,Hint 6,Hint 7,Hint 8,Hint 1,Hint 9,Hint 4,Hint 1,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 2,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 5,Empty,Hint 4,Empty,Hint 7,Empty,Empty,Hint 8,Empty,Empty,Empty,Hint 3,Empty,Empty,Empty,Empty,Hint 1,Empty,Hint 9,Empty,Empty,Empty,Empty,Hint 3,Empty,Empty,Hint 4,Empty,Empty,Hint 2,Empty,Empty,Empty,Hint 5,Empty,Hint 1,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 8,Empty,Hint 6,Empty,Empty,Empty]}"
