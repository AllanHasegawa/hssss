import Data.List
import SudokuUtils as SU

main = do
	SU.solveProblem 0 mySolver
	-- print $ head problem 

mySolver :: SudokuBoard -> SudokuBoard
mySolver sBoard = sBoard

-- solve (Empty):xs 

genGuesses :: SudokuBoard -> Int -> SudokuUnit
genGuesses sBoard index = let
	sBS = bS sBoard
	u = getUniques sBoard index
	aGuesses = [1..sBS]
	in Guess [x | x <- aGuesses, (Hint x) `notElem` u]

getUniques :: SudokuBoard -> Int -> [SudokuUnit]
getUniques sBoard index = let
	uSS = uniqueSudokuUnits $ getSS (iBStoSS sBoard index) sBoard
	uCol = uniqueSudokuUnits $ getColBS (iBStoCol sBoard index) sBoard
	uRow = uniqueSudokuUnits $ getRowBS (iBStoRow sBoard index) sBoard
	in nub $ uSS `union` uCol `union` uRow

iBStoCol :: SudokuBoard -> Int -> Int
iBStoCol sBoard index = index `mod` (bS sBoard)

iBStoRow :: SudokuBoard -> Int -> Int
iBStoRow sBoard index = index `div` (bS sBoard)

iBStoSS :: SudokuBoard -> Int -> Int
iBStoSS sBoard index = let
	c = iBStoCol sBoard index
	r = iBStoRow sBoard index
	s = bS sBoard
	i = sS sBoard
	in ((r `div` i) * i) + (c `div` i)
