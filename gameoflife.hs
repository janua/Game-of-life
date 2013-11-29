--Game of life

import Data.List.Split (chunksOf)
import Data.List (intercalate)

width :: Int
width = 10

height:: Int
height = 10

deadCharacter :: Char
deadCharacter = ' '

aliveCharacter :: Char
aliveCharacter = 'x'

main = do
	putStrLn "Game of life!"
	printBoard $ generateBoard width height

type Board = [Cell]

data Cell = Dead | Alive

generateBoard :: Int -> Int -> Board
generateBoard x y = replicate size Alive
	where size = x * y


printBoard :: Board -> IO ()
printBoard b = mapM_ putChar $ intercalate ['\n'] $ chunksOf width converted
	where converted = map printCell b

printCell :: Cell -> Char
printCell c = case c of
	Dead 	-> deadCharacter
	Alive 	-> aliveCharacter
