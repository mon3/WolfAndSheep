-- module Data

-- where

import System.Random
import Data.Time.Clock
import Data.Time.Calendar

data Point = Point Int Int deriving (Eq,Show)

data Wolf = Wolf Point deriving Show
data Sheep = Sheep [Point] deriving (Eq,Show)
data GameState = State Wolf Sheep deriving Show
data Result = WolfWins | SheepWins | Unrecognized deriving Show



initialize :: [Point] -> [Point] -> GameState
initialize wolfStates sheepStates = State wolf sheep
	where 
	wolfState = (wolfStates!!1)
	wolf = Wolf wolfState
	sheep = Sheep sheepStates
									
-- wolf = Wolf () 
-- sheep = Sheep sheepInitStates
-- initGameameState = State wolf sheep



-- atRandIndex :: [Point]  -> Point  -- note that this is gives itself an IO action
-- atRandIndex []  = (Point 1 1) 
-- atRandIndex l  =  fmap (l !!) $ randomRIO (0, length l - 1)  :: Point

-- res = atRandIndex wolfInitStates

wolfInitStates = [Point x y | y <- [7], x <- [0..7], odd x] 
sheepInitStates = [Point x y | y <- [0], x <- [0..7], even x]
main = print(initialize wolfInitStates sheepInitStates)

