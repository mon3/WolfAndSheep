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


moveSheep :: GameState -> Int -> Point -> GameState
moveSheep (GameState wolf sheep) idx delta = (GameState wolf (Sheep (changeSheepPosition sheep idx delta)))


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
-- main = print(initialize wolfInitStates sheepInitStates)
gameState = initialize wolfInitStates sheepInitStates
-- main = print(moveWolf gameState (Point 1 (-1)))
main = print(moveSheep gameState 3 (Point 1 (1))) -- owce o indeksach [0;3] poruszaja sie po planszy
