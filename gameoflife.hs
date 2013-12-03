--Game of life

module Main where

import Data.List.Split (chunksOf)
import Data.List (intercalate)
import System.Environment

width :: Int
width = 10

height:: Int
height = 10

deadCharacter :: Char
deadCharacter = ' '

aliveCharacter :: Char
aliveCharacter = 'x'

main :: IO ()
main = do
	putStrLn "Game of life!"
	testBoard : iterations : [] <- getArgs
	let b = take (read iterations) $ iterate cycleBoard $ getBoard $ read testBoard
	mapM_ (\board -> putStrLn "\n-----" >> printBoard board) b


type Board = [Cell]

type Coord = (Int, Int)

data Cell = Dead | Alive deriving (Eq)

getBoard :: Int -> Board
getBoard 1 = testBoardStatic
getBoard 2 = testBoardBlinkers
getBoard _ = testGlider

generateBoard :: Int -> Int -> Board
generateBoard x y = replicate size Dead
	where size = x * y


printBoard :: Board -> IO ()
printBoard b = mapM_ putChar $ intercalate ['\n'] $ chunksOf width converted
	where converted = map printCell b

printCell :: Cell -> Char
printCell c = case c of
	Dead 	-> deadCharacter
	Alive 	-> aliveCharacter


cycleBoard :: Board -> Board
cycleBoard board = map (\coord -> rule (getCell coord board) $ neighbours coord board) allCoords
	where allCoords = do
		y <- [0..height-1]
		x <- [0..width-1]
		return (x, y)

rule :: Cell -> [Cell] -> Cell
rule currentCell cells = case currentCell of 
						Dead 	| aliveCells == 3 -> Alive
								| otherwise -> Dead
						Alive 	| aliveCells == 2 || aliveCells == 3 -> Alive
								| otherwise -> Dead 
	where aliveCells = length $ filter (\c -> c == Alive) cells

neighbours :: Coord -> Board -> [Cell]
neighbours coord board = 
	getCell (topLeft coord) board
	: getCell (up coord) board
	: getCell (topRight coord) board
	: getCell (left coord) board
	: getCell (right coord) board
	: getCell (bottomLeft coord) board
	: getCell (down coord) board
	: getCell (bottomRight coord) board
	: []

left :: Coord -> Coord
left (x, y) =
	let x' = x - 1
	in case () of _
					| x' < 0  	-> (width - 1, y)
					| otherwise -> (x', y)

right :: Coord -> Coord
right (x, y) =
	let x' = x + 1
	in case () of _
					| x' >  width - 1  	-> (0, y)
					| otherwise 		-> (x', y)

up :: Coord -> Coord
up (x, y) =
	let y' = y - 1
	in case () of _
					| y' < 0	-> (x, height - 1)
					| otherwise	-> (x, y')

down :: Coord -> Coord
down (x, y) =
	let y' = y + 1
	in case () of _
					| y' > height - 1	-> (x, 0)
					| otherwise		-> (x, y')

topLeft :: Coord -> Coord
topLeft coord = left $ up coord

topRight :: Coord -> Coord
topRight coord = right $ up coord

bottomLeft :: Coord -> Coord
bottomLeft coord = left $ down coord

bottomRight :: Coord -> Coord
bottomRight coord = right $ down coord

getCell :: Coord -> Board -> Cell
getCell (x, y) board = board !! position 
	where position = x + (width * y)

--TestData
testBoardStatic :: Board
testBoardStatic =
	[Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Alive, Alive, Dead, Dead]
	++ [Dead, Alive, Alive, Dead, Dead, Alive, Dead, Dead, Alive, Dead]
	++ [Dead, Alive, Alive, Dead, Dead, Dead, Alive, Alive, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Alive, Alive, Dead, Dead, Dead, Alive, Alive, Dead, Dead]
	++ [Alive, Dead, Dead, Alive, Dead, Dead, Alive, Dead, Alive, Dead]
	++ [Dead, Alive, Dead, Alive, Dead, Dead, Dead, Alive, Dead, Dead]
	++ [Dead, Dead, Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead]

testBoardBlinkers :: Board
testBoardBlinkers =
	[Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Alive, Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead]
	++ [Dead, Alive, Dead, Dead, Alive, Dead, Dead, Alive, Dead, Dead]
	++ [Dead, Alive, Dead, Dead, Alive, Dead, Dead, Alive, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Alive, Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Alive, Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Alive, Alive, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Alive, Alive, Dead, Dead, Dead, Dead, Dead]

testGlider :: Board
testGlider =
	[Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Alive, Dead, Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Alive, Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
	++ [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
