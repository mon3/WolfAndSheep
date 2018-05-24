module Model where

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
boardBoundaryHorizontal = [Point x y | x<- [0..7], y <- [0,7]]
boardBoundaryVertical = [Point x y | x<- [0,7], y <- [1..6]]
boardBoundaries = boardBoundaryVertical ++ boardBoundaryHorizontal
boardXY = [0,7]

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

possibleNewWolfPositions :: GameState -> [Point]
possibleNewWolfPositions (GameState (Wolf wolf) sheep) = [(moveHero wolf point) | point <- wolfMoves]

-- zwraca True, jesli ktoras z owiec zajmuje juz wskazane miejsce
checkUsedSheepPositions :: Sheep -> Point -> Bool
checkUsedSheepPositions (Sheep sheeps) newPoint = if elem True positionList then True
                                                  else False
                                                       where
                                                          positionList = map (comparePositions newPoint) sheeps



validMove :: Point -> [Int] -> Bool
validMove point [low, high] = if (x >= low) && (x<=high) && (y>=low) && (y<=high) then True
                              else False
                                   where
                                      x = (xPoint point)
                                      y = (yPoint point)



checkPointWithinBoard :: Point -> [Point] -> Bool
checkPointWithinBoard _ [] = True
checkPointWithinBoard point (current:rest) = if (((xPoint point) <= (xPoint current)) && ((yPoint point) <= (yPoint current))) then (checkPointWithinBoard point rest)
                                             else False


moveSheep :: GameState -> Int -> Point -> GameState
moveSheep (GameState wolf sheep) idx delta = (GameState wolf (Sheep (changeSheepPosition sheep idx delta)))



validWolfMove :: GameState -> Point -> Bool
validWolfMove (GameState (Wolf wolf) sheep) delta = if  ((checkUsedSheepPositions sheep newPosition)==False) && (validMove newPosition boardXY) then True
                                                    else False
                                                         where newPosition = (moveHero wolf delta)

--validWolfMovePosition :: GameState -> Point -> Bool
--validWolfMovePosition (GameState (Wolf wolf) sheep) delta = if  ((checkUsedSheepPositions sheep delta)==False) && (validMove newPosition boardXY) then True
--                                                    else False
--                                                         where newPosition = (moveHero wolf delta)


-- -- z danego stanu chcemy przejsc owca o wskazanym indeksie o delte: sprawdzamy czy nie koliduje z innymi owcami oraz wilkiem
validSheepMove :: GameState -> Int -> Point -> Bool
validSheepMove (GameState (Wolf wolf) (Sheep (sheep:rest))) 0 delta = if ((checkUsedSheepPositions (Sheep rest) newSheepPosition)==False) && (validMove newSheepPosition boardXY) && ((comparePositions wolf sheep)==False) then True
                                                            else False
                                                                 where newSheepPosition = (moveHero sheep delta)
validSheepMove (GameState (Wolf wolf)(Sheep (sheep:rest))) idx delta = validSheepMove (GameState (Wolf wolf) (Sheep(rest++[sheep]))) (idx-1) delta


getWolfMove :: GameState -> Point
getWolfMove (GameState (Wolf wolf) sheep) = wolf