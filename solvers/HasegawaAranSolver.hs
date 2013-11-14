import SudokuUtils as SU

myf :: SudokuBoard -> SudokuBoard
myf f = f

main = do
	SU.solveProblem 44900 myf
	-- print $ head problem 
