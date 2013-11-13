import SudokuInput as SI

myf :: [[Int]] -> [[Int]]
myf f = f

main = do
	problem <- SI.solveProblem 44900 myf
	print problem
