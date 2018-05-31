module Controller where
import Model

--TODO: change implementation
getFirstStep :: GameInfo
getFirstStep = GameInfo (GameState (Wolf (Point 1 1)) (Sheep sheepInitStates)) Unrecognized



--TODO: change implementation
updateGameInfo :: GameInfo -> GameInfo
updateGameInfo gI = gI


---------------------------INITIALIZATION---------------------------------------------
wolfInitStates = [Point x y | y <- [7], x <- [0..7], odd x]
sheepInitStates = [Point x y | y <- [0], x <- [0..7], even x]
sheep = Sheep sheepInitStates