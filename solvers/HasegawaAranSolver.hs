module HasegawaAranSolver
(
mySolver
) where

import Data.List
import SudokuUtils as SU
import Debug.Trace

main = do
	SU.solveProblem 0 mySolver
	-- print $ head problem 

mySolver :: SudokuBoard -> [SudokuBoard]
mySolver sBoard = let
	sBS = bS sBoard
	in solve sBoard sBoard False 0 (sBS*sBS)

lenGuesses (Guess x) = length x
remGuess (Guess (x:xs)) = Guess xs

-- solve oBoard uBoard False n end | trace ("solve " ++ (show n)) False = undefined

solve oBoard uBoard backing n end
	| n == -1 = []
	| n == end = uBoard:(solve oBoard uBoard True (n-1) end)
	| isEmpty x = let
		g = genGuesses uBoard n
		l = lenGuesses g
		backBoard = drop (n+1) $ board oBoard
		u = updateBoard uBoard n (g:backBoard)
		in if l > 0 then
      			solve oBoard u False (n+1) end
	   	   else
			solve oBoard uBoard True (n-1) end 
	| isHint x = if backing then
			 solve oBoard uBoard True (n-1) end
		     else
			 solve oBoard uBoard False (n+1) end
	| isGuess x = let
		l = lenGuesses x
		backBoard = drop (n+1) $ board oBoard
		u = updateBoard uBoard n ((remGuess x):backBoard)
		in if l > 1 then
	      		solve oBoard u False (n+1) end
		   else
	      		solve oBoard uBoard True (n-1) end
	where x = (board uBoard) !! n

genGuesses :: SudokuBoard -> Int -> SudokuUnit
genGuesses sBoard index = let
	sBS = bS sBoard
	u = getUniques sBoard index
	aGuesses = [1..sBS]
	in Guess [x | x <- aGuesses, (Hint x) `notElem` u]

getUniques :: SudokuBoard -> Int -> [SudokuUnit]
getUniques sBoard index = let
	uSS = getSS (iBStoSS sBoard index) sBoard
	uCol = getColBS (iBStoCol sBoard index) sBoard
	uRow = getRowBS (iBStoRow sBoard index) sBoard
	in uniqueSudokuUnits $ uSS ++ uCol ++ uRow

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
