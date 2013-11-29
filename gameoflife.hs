--Game of life

main = do
	putStrLn "Game of life!"
	printBoard $ generateBoard 10 10

type Board = [Cell]

data Cell = Dead | Alive

generateBoard :: Int -> Int -> Board
generateBoard x y = replicate size Dead
	where size = x * y


printBoard :: Board -> IO ()
printBoard b = mapM_ putChar $ map printCell b

printCell :: Cell -> Char
printCell c = case c of
	Dead -> ' '
	Alive -> 'x'
