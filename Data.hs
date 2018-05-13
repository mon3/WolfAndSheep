-- module Data

-- where

import System.Random
import Data.Time.Clock
import Data.Time.Calendar

data Point = Point Int Int deriving (Eq,Show)

data Wolf = Wolf Point deriving Show
data Sheep = Sheep [Point] deriving (Eq,Show)
data GameState = GameState Wolf Sheep deriving Show
data Result = WolfWins | SheepWins | Unrecognized deriving Show

possibleBoardX = [0..7]
possibleBoardY = [0..7]
wolfMoves = [Point x y | x <- [1,-1], y <- [1,-1]] -- przesunięcia wilka
sheepMoves = [Point x y | x <- [1,-1], y <- [1]] -- przesunięcia owcy

sheepNumber = [0..3]

xPoint :: Point -> Int
xPoint (Point x y) = x

yPoint :: Point -> Int
yPoint (Point x y) = y

moveHero :: Point -> Point -> Point
moveHero current delta = Point (xPoint(current) + xPoint(delta)) (yPoint(current)+yPoint(delta))


moveWolf :: GameState -> Point -> GameState
moveWolf (GameState (Wolf position) sheep) delta = (GameState (Wolf (moveHero position delta)) sheep)


changeSheepPosition :: Sheep -> Int -> Point -> [Point]
changeSheepPosition (Sheep (position:sheeps)) 0 delta =  ((moveHero position delta):sheeps)
changeSheepPosition (Sheep (position:sheeps)) idx delta =  (position: (changeSheepPosition (Sheep sheeps) (idx-1) delta))


comparePositions :: Point -> Point -> Bool
comparePositions point1 point2 = if point1==point2 then True
								 else False


checkUsedSheepPositions :: Sheep -> Point -> Bool
checkUsedSheepPositions (Sheep sheeps) delta = if elem True positionList then True
											   else False
											   where 
											   positionList = map (comparePositions delta) sheeps




moveSheep :: GameState -> Int -> Point -> GameState
moveSheep (GameState wolf sheep) idx delta = (GameState wolf (Sheep (changeSheepPosition sheep idx delta)))


validWolfMove :: GameState -> Point -> Bool
validWolfMove (GameState (Wolf wolf) sheep) delta = if  checkUsedSheepPositions sheep delta == True then False
													else True

-- validSheepMove :: GameState -> Point -> Bool
-- validSheepMove (GameState (Wolf wolf) sheep) delta = if  checkUsedSheepPositions sheep delta == True then False
-- 													else True



initialize :: [Point] -> [Point] -> GameState
initialize wolfStates sheepStates = GameState wolf sheep
	where 
	wolfState = (wolfStates!!1)
	wolf = Wolf wolfState
	sheep = Sheep sheepStates
									


-- atRandIndex :: [Point]  -> Point  -- note that this is gives itself an IO action
-- atRandIndex []  = (Point 1 1) 
-- atRandIndex l  =  fmap (l !!) $ randomRIO (0, length l - 1)  :: Point

-- res = atRandIndex wolfInitStates





---------------------------INITIALIZATION---------------------------------------------
wolfInitStates = [Point x y | y <- [7], x <- [0..7], odd x] 
sheepInitStates = [Point x y | y <- [0], x <- [0..7], even x]
sheep = Sheep sheepInitStates
-- main = print(initialize wolfInitStates sheepInitStates)
gameState = initialize wolfInitStates sheepInitStates
-- main = print(moveWolf gameState (Point 1 (-1)))
-- main = print(moveSheep gameState 3 (Point 1 (1))) -- owce o indeksach [0;3] poruszaja sie po planszy
-- main = print(map comparePositions [(Point 1 1) (Point 2 2)] (Point 1 1))

-- main = print(sheep)
main = print(validWolfMove gameState (Point 1 1))