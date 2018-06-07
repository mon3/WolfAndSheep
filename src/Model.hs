module Model where

import Data.Time.Clock
import Data.Time.Calendar

data Point = Point Int Int deriving (Eq,Show,Read)

data Wolf = Wolf Point deriving (Eq,Show,Read)
data Sheep = Sheep [Point] deriving (Eq,Show,Read)
data GameState = GameState Wolf Sheep deriving (Eq, Show,Read)
data Result = WolfWins | SheepWins | Unrecognized deriving (Eq,Show,Read)
data GameInfo = GameInfo {state :: GameState, result :: Result } deriving (Eq,Show,Read)



possibleBoardX = [0..7]
possibleBoardY = [0..7]
wolfMoves = [Point x y | x <- [1,-1], y <- [1,-1]] -- possible wolf moves
sheepMoves = [Point x y | x <- [1,-1], y <- [1]] -- possible sheep moves
boardBoundaryHorizontal = [Point x y | x<- [0..7], y <- [0,7]]
boardBoundaryVertical = [Point x y | x<- [0,7], y <- [1..6]]
boardBoundaries = boardBoundaryVertical ++ boardBoundaryHorizontal -- no one is able to cross board boundarie
boardXY = [0,7]

-- sheeo iterator
sheepNumber = [0..3]


xPoint :: Point -> Int
xPoint (Point x y) = x


yPoint :: Point -> Int
yPoint (Point x y) = y


distance :: Point -> Point -> Point
distance point1 (Point 0 0) = (Point (abs(xPoint point1)) (abs(yPoint point1)))
distance (Point 0 0) point2 = (Point (abs(xPoint point2)) (abs(yPoint point2)))
distance point1 point2 = (Point (abs((xPoint point1) - (xPoint point2))) (abs((yPoint point1) - (yPoint point2))))


-- Euclidean norm
pointVal :: Point -> Int
pointVal (Point x y) = round (sqrt(fromIntegral x^2+ fromIntegral y^2))


-- moves game hero(sheep/wolf) by the delta point(one of sheepMoves/wolfMoves)
moveHero :: Point -> Point -> Point
moveHero current delta = Point (xPoint(current) + xPoint(delta)) (yPoint(current)+yPoint(delta))


moveWolf :: GameState -> Point -> GameState
moveWolf (GameState (Wolf position) sheep) delta = (GameState (Wolf (moveHero position delta)) sheep)


changeSheepPosition :: Sheep -> Int -> Point -> [Point]
changeSheepPosition (Sheep (position:sheeps)) 0 delta =  ((moveHero position delta):sheeps)
changeSheepPosition (Sheep (position:sheeps)) idx delta =  (position: (changeSheepPosition (Sheep sheeps) (idx-1) delta))


-- compares two heros' positions (as (x1,y1) and (x2,y2))
comparePositions :: Point -> Point -> Bool
comparePositions point1 point2 = if point1==point2 then True
                                 else False


-- generates possible New Wolf Positions from given GameState: list of new possible Points returned
possibleNewWolfPositions :: GameState -> [Point]
possibleNewWolfPositions (GameState (Wolf wolf) sheep) = [(moveHero wolf point) | point <- wolfMoves]


-- generates new possible Sheep positions
possibleNewSheepPositions :: GameState -> [Sheep]
possibleNewSheepPositions (GameState (Wolf wolf) sheep) = [Sheep (changeSheepPosition sheep idx point) | idx <- sheepNumber, point <- sheepMoves, (validSheepMove (GameState (Wolf wolf) sheep) idx point)]


-- True if one of the sheeps is already at given Point (input param)
checkUsedSheepPositions :: Sheep -> Point -> Bool
checkUsedSheepPositions (Sheep sheeps) newPoint = if elem True positionList then True
                                                  else False
                                                       where
                                                          positionList = map (comparePositions newPoint) sheeps


-- checks is move is valid between game board boundaries
validMove :: Point -> [Int] -> Bool
validMove point [low, high] = if (x >= low) && (x<=high) && (y>=low) && (y<=high) then True
                              else False
                                   where
                                      x = (xPoint point)
                                      y = (yPoint point)


-- check if point is within board boundaries provided as a list
checkPointWithinBoard :: Point -> [Point] -> Bool
checkPointWithinBoard _ [] = True
checkPointWithinBoard point (current:rest) = if (((xPoint point) <= (xPoint current)) && ((yPoint point) <= (yPoint current))) then (checkPointWithinBoard point rest)
                                             else False

-- moves sheep by the displacement (one of possible Points from sheepMoves list)
moveSheep :: GameState -> Int -> Point -> GameState
moveSheep (GameState wolf sheep) idx delta = (GameState wolf (Sheep (changeSheepPosition sheep idx delta)))

-- check if wolf move from current GameState by displacement (Point from wolfMoves vector) is valid
validWolfMove :: GameState -> Point -> Bool
validWolfMove (GameState (Wolf wolf) sheep) delta = if  ((checkUsedSheepPositions sheep newPosition)==False) && (validMove newPosition boardXY) then True
                                                    else False
                                                         where newPosition = (moveHero wolf delta)


-- move sheep from current GameState by given displacement (Point from sheepMoves): checks if does not collide with other sheeps or eolf
validSheepMove :: GameState -> Int -> Point -> Bool
validSheepMove (GameState (Wolf wolf) (Sheep (sheep:rest))) 0 delta = if ((checkUsedSheepPositions (Sheep rest) newSheepPosition)==False) && (validMove newSheepPosition boardXY) && ((comparePositions wolf newSheepPosition)==False) then True
                                                            else False
                                                                 where newSheepPosition = (moveHero sheep delta)
validSheepMove (GameState (Wolf wolf)(Sheep (sheep:rest))) idx delta = validSheepMove (GameState (Wolf wolf) (Sheep(rest++[sheep]))) (idx-1) delta


-- get wolf move from given GameState
getWolfMove :: GameState -> Point
getWolfMove (GameState (Wolf wolf) sheep) = wolf


getSheepAcceptableMoves :: GameState -> [[Point]]
getSheepAcceptableMoves (GameState (Wolf wolf) (Sheep (sheep:rest))) = take ((length proposed) - 1) proposed
                                                                       where proposed = (filterProposalMoveForSheep (GameState (Wolf wolf) (Sheep (sheep:rest))) 0)
                                                                                        where filterProposalMoveForSheep (GameState (Wolf wolf) (Sheep (sheep:rest))) idx  | idx < (length (sheep:rest)) = [ map (moveHero sheep) (filter (validSheepMove ((GameState (Wolf wolf) (Sheep (sheep:rest)))) 0) (sheepMoves))] ++ (filterProposalMoveForSheep (GameState (Wolf wolf) (Sheep (rest++[sheep]))) (idx+1))
                                                                                                                                                                   | otherwise = [[]]