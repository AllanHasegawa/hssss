module SudokuUtils
(
SudokuBoard(..),
SudokuUnit(..),
solveProblem,
updateBoard,
verifySolution,
isGuess,
isHint,
isEmpty,
getColsBS,
getRowsBS,
getColBS,
getRowBS,
getSS,
getSSs,
uniqueSudokuUnits,
getPureProblem,
nub'
) where

import System.IO
import Data.Char
import Data.List
import qualified Data.Set as S

data SudokuUnit =
	Empty |
	Hint Int |
	Guess [Int]
	deriving (Show,Read)

instance Eq SudokuUnit where
	Empty == Empty = True
	Hint x == Hint y = x == y
	Hint x == Guess (y:ys) = x == y
	Guess (x:xs) == Guess (y:ys) = x == y
	Guess (x:xs) == Hint y = x == y
	Guess [] == Guess [] = True
	_ == _ = False

instance Ord SudokuUnit where
	compare Empty Empty = EQ
	compare Empty _ = LT
	compare _ Empty = GT
	compare (Guess []) _ = LT
	compare _ (Guess []) = GT
	compare (Guess (x:xs)) (Guess (y:ys)) = compare x y
	compare (Guess (x:xs)) (Hint y) = compare x y
	compare (Hint x) (Hint y) = compare x y
	compare (Hint x) (Guess (y:ys)) = compare x y

-- sS :: size of the small square
-- bS :: size of the big square
-- board :: list of SudokuUnit's
data SudokuBoard = SudokuBoard {
				sS :: Int,
				bS :: Int,
				board :: [SudokuUnit]
			} deriving (Show,Read)


solveProblem :: Int -> (SudokuBoard -> [SudokuBoard]) -> IO ()
solveProblem problemIndex solver
	| problemIndex >= 0 = (do
		handle <- openFile "../problems/sudoku17.txt" ReadMode
		contents <- hGetContents handle
		let sudokuLine = lines contents !! problemIndex
		let sudokuBoard = stringToSudokuBoard sudokuLine
      		let sBoardSolved = solver sudokuBoard
		putStr $ sudokuBoardToFancyString sudokuBoard
		putStr $ show $ length sBoardSolved
		putStr $ concat $ map sudokuBoardToFancyString sBoardSolved
		--print $ verifySolution sBoardSolved
		hClose handle
		)
	| otherwise = print "WAT?"


updateBoard :: SudokuBoard -> Int -> [SudokuUnit] -> SudokuBoard
updateBoard sBoard index sUnits = let
	sBS = bS sBoard
	sSS = sS sBoard
	b = board sBoard
	newSUnits = take index b ++ sUnits
	in SudokuBoard sSS sBS newSUnits

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

isGuess (Guess _) = True
isGuess _ = False

isHint (Hint _) = True
isHint _ = False

isEmpty Empty = True
isEmpty _ = False

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
uniqueSudokuUnits sUnits = nub' $ filter (\x -> x /= Empty) sUnits

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
sudokuUnitToString (Guess (x:xs)) = show x

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

-- nub with (Ord) by http://buffered.io/posts/a-better-nub/
nub' :: (Ord a) => [a] -> [a]
nub' = go S.empty
	where 	go _ [] = []
       		go s (x:xs)
		 | S.member x s = go s xs
		 | otherwise    = x : go (S.insert x s) xs

getPureProblem :: SudokuBoard
getPureProblem = read "SudokuBoard {sS = 3, bS = 9, board = [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 1,Empty,Hint 4,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 2,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 5,Empty,Hint 4,Empty,Hint 7,Empty,Empty,Hint 8,Empty,Empty,Empty,Hint 3,Empty,Empty,Empty,Empty,Hint 1,Empty,Hint 9,Empty,Empty,Empty,Empty,Hint 3,Empty,Empty,Hint 4,Empty,Empty,Hint 2,Empty,Empty,Empty,Hint 5,Empty,Hint 1,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Hint 8,Empty,Hint 6,Empty,Empty,Empty]}"
