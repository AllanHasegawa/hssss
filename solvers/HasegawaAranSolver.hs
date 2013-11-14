import SudokuInput as SI

myf :: SudokuBoard -> SudokuBoard
myf f = f

main = do
	problem <- SI.solveProblem 44900 myf
	print problem
	-- print $ head problem 
