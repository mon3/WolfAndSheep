module AI where

import Model


g:: Int -> String
g _ = "Success!"

data GameTree a = Empty | Node a [GameTree a] deriving (Show)

data ScoreTree a = GameTree a

minmaxDepth = 4

minimumValue = -100
maximumValue = 100


depth :: GameTree a -> Int
depth Empty = 0
depth (Node _ []) = 1
depth (Node _ ts) = 1 + maximum(map depth ts)


-- checks if wolf wins in currentGamestate or will win in the next step
wolfWon :: GameState -> Point -> Bool
wolfWon (GameState (Wolf (Point _ 0)) sheep) _ = True
wolfWon gs dpoint = if (validWolfMove gs dpoint) && (yPoint newPos == 0) then True else False
                    where newPos = getWolfMove (moveWolf gs dpoint)


-- generates next GameStates if wolf moves altogether with calculated heuristics
getWolfNextMoves :: GameState -> [(GameState, Int)]
getWolfNextMoves (GameState (Wolf wolf) sheep) = [ (GameState (Wolf (moveHero wolf point)) sheep, getHeuristic ((GameState (Wolf (moveHero wolf point)) sheep))) | point <- wolfMoves, (validWolfMove (GameState (Wolf wolf) sheep) point)]


-- generates next GameStates if any of sheeps moves altogether with calculated heuristics
getSheepNextStates :: GameState -> [(GameState, Int)]
getSheepNextStates (GameState wolf sheep) = [((GameState wolf newSheep), (getHeuristic (GameState wolf newSheep))) | newSheep <- (possibleNewSheepPositions (GameState wolf sheep))]


-- checks if last wolf move is winning (in case 1 of 4 possible wolf movws is chosen)
lastWolfMove :: GameState -> Bool
lastWolfMove gs = if elem True resList then True
                  else False
                    where resList = map (wolfWon gs) x
                          x = [a | a<-wolfMoves]


-- heuristic evaluation of current GameState
-- key factors influencing game result: distance to row (_,0); x distance to game board center; number of possible
-- directions wolf would be able to move; number of sheeps surrounding wolf: more sheeps -> less valid wolf moves
getHeuristic :: GameState -> Int
getHeuristic (GameState (Wolf (Point _ 0)) sheep) = 0
getHeuristic (GameState (Wolf (Point x y)) sheep) = distanceToFinish * 10 + sheepLocalization + possibleDirections*10 + distanceToCenter * 10
                                                 where distanceToFinish = abs(y)
                                                       distanceToCenter = if x<=4 then abs(0-x) else abs(7-x)
                                                       possibleDirections = length [ point | point <- wolfMoves, (validWolfMove (GameState (Wolf (Point x y)) sheep) point)]
                                                       sheepLocalization = if sheepSurrounding (GameState (Wolf (Point x y)) sheep) == 4 then -1000
                                                                           else sheepSurrounding (GameState (Wolf (Point x y)) sheep)


-- calculates distance between wold and sheeps at current GameState
sheepSurrounding :: GameState -> Int
sheepSurrounding (GameState (Wolf wolf) (Sheep [])) = 0
sheepSurrounding (GameState (Wolf wolf) (Sheep (sheep:rest))) = pointVal (distance wolf sheep) + (sheepSurrounding (GameState (Wolf wolf) (Sheep rest)))


-- chooses best move with assigned evaluated heuristics
-- 0: sheep, 1 : wolf (1st parameter)
bestMove :: Int -> (GameState, Int) -> [(GameState, Int)] -> (GameState, Int)
bestMove _ best [] = best
bestMove 0 (bestGameState, bestHeuristic) ((gameState, val):others) | (bestHeuristic <= val) = bestMove 0 (bestGameState, bestHeuristic) others
                                                                       | otherwise = bestMove 0 (gameState, val) others
bestMove 1 (bestGameState, bestHeuristic) ((gameState, val):others) | (bestHeuristic >= val) = bestMove 1 (bestGameState, bestHeuristic) others
                                                                       | otherwise = bestMove 1 (gameState, val) others


-- gets heuristics of selected new GameState (after the move is finished)
getMoveValue :: (GameState, Int) -> Int
getMoveValue (gs, x) = x


-- selects best wolf Move from given possible wolf moves list and returns best heuristics evaluated
-- from wolf perspective (MAX)
-- random GameState as initial gs just to be able to compare score for gameStates given as an input list
bestWolfMoveValue :: [(GameState, Int)] -> Int
bestWolfMoveValue list = value where
  (_,value) = bestMove 1 ((GameState (Wolf (Point 3 5)) (Sheep [Point 2 4,Point 4 4,Point 2 6,Point 4 6])), -1000) list


-- selects and returns best wolf Move (new GameState0 from given possible wolf moves list evaluated
-- from wolf perspective (MAX)
bestWolfMove :: [(GameState, Int)] -> GameState
bestWolfMove list = gs where
  (gs,_) = bestMove 1 ((GameState (Wolf (Point 3 5)) (Sheep [Point 2 4,Point 4 4,Point 2 6,Point 4 6])), -1000) list


-- depth, GameState, Point (wolf displacement)
-- 2nd case - wolf wins
minmaxWolfMove :: Int -> GameState -> Point -> (GameState, Int)
minmaxWolfMove 0 gs displacement = (gs, (getHeuristic (moveWolf gs displacement)))
minmaxWolfMove i (GameState (Wolf (Point x y)) sheep ) (Point dx dy) | (y+dy) == 0 = ((moveWolf (GameState (Wolf (Point x y)) sheep ) (Point dx dy)), maximumValue)
minmaxWolfMove i gs displacement =
   if (null moves)
   then (moveWolf gs displacement, 0)
   else (moveWolf gs displacement, (minimum  moves))
   where
   moves = [minmaxSheepMove (i-1) (moveWolf gs displacement) sheepid sheepVector | sheepid <- sheepNumber, sheepVector <- sheepMoves, validSheepMove (moveWolf gs displacement) sheepid sheepVector]


-- returns heuristics evaluation after one of sheeps moves (all possible GameStates taken into account) - returns the
-- one that is supposed to be the best for wolf
minmaxSheepMove :: Int -> GameState -> Int -> Point -> Int
minmaxSheepMove i state sheepid displacement =
  if (null wolfMoves) then
   0
  else
   bestWolfMoveValue wolfNewMoves -- returns best wolf move(according to heuristics) from all possible moves that are
--   valid from current game tree level
  where
   newState = moveSheep state sheepid displacement
   wolfNewMoves = [minmaxWolfMove i newState wolfVector | wolfVector <- wolfMoves, validWolfMove newState wolfVector]


-- best wolf MIN-MAX move from current GameState
wolfGameMove :: GameState -> GameState
wolfGameMove gs = (bestWolfMove moves) where
                       moves = [minmaxWolfMove minmaxDepth gs displacement | displacement <- wolfMoves, validWolfMove gs displacement]



-- start of MIN-MAX algorithm which selects best wolf move
-- return GameDTO (new gamestate, game result)
wolfMoveState :: GameDTO -> GameDTO
wolfMoveState (GameDTO (gs, res)) =
  if (null possibleMoves)
  then
   GameDTO (gs,SheepWins)
  else
   if (lastWolfMove gs)
   then
    GameDTO (gs, WolfWins)
   else
    GameDTO (wolfGameMove gs, Unrecognized)
  where
   possibleMoves = [displacement | displacement <- wolfMoves, validWolfMove gs displacement]





-- builds game Tree from given GameState
--buildGameTree :: GameState -> GameTree GameState
--buildGameTree gs  | (lastWolfMove gs)  = Node gs []
--                  | otherwise = Node gs [(buildGameTree state) | state <- (getWolfNextMoves gs), (depth (buildGameTree state) <= 2) ]
----                  | otherwise = Node gs [(buildGameTree state) | state <- (getWolfNextMoves gs) ]
--
----                  (map (fmap buildGameTree) childWolfMoves)
--                    where
--                      possibleMoves = getWolfNextMoves gs
--                  childWolfMoves =  [(moveWolf a ) | a <- possibleMoves]
--                  map(\move -> moveWolf gs move) possibleMoves


--prune :: Int -> GameTree a -> GameTree a
--prune 0 (Node a _) = Node a []
--prune d (Node a xs) = Node a (map (prune (d - 1)) xs)


