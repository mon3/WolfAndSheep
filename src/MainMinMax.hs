--module MainMinMax where
import Model
import AI
import Controller


---------------------------INITIALIZATION---------------------------------------------

-- to test if wolf move is valid
main = do
    gameState <- (initialize wolfInitStates sheepInitStates)
    print(gameState)
    print(getHeuristic (GameState (Wolf (Point 3 5)) (Sheep [Point 2 4,Point 4 4,Point 2 6,Point 4 6])))

    print(validSheepMove gameState 0 (Point (-1) 1))
--    let val = (getHeuristic gameState)
    let newPos =[ moveHero (getWolfMove gameState) point | point <- wolfMoves, (validWolfMove gameState point)]
    let res =  wolfWon (GameState (Wolf (Point 7 1)) (Sheep [Point 4 4,Point 2 4,Point 0 4,Point 0 6])) (Point (-1) (-1))
    print( (possibleNewSheepPositions (GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6]))))
    print(getSheepNextStates (GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6])))
    print(bestMove 0 ((GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6])),1000) (getWolfNextMoves (GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6]))))
    print(GameInfo gameState Unrecognized)
    print(validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 5 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1))
    print([getHeuristic (GameState (Wolf (Point 4 4)) sheep) | sheep <- (possibleNewSheepPositions (GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6])))])
    let newState = moveSheep gameState 0 (Point (1) 1)
    print( [minmaxWolfMove 2 newState wolfVector | wolfVector <- wolfMoves, validWolfMove newState wolfVector])
    print(bestWolfMoveValue [minmaxWolfMove 2 newState wolfVector | wolfVector <- wolfMoves, validWolfMove newState wolfVector])
--    WOLF WINS
--    print(wolfMoveState (GameInfo (GameState (Wolf (Point 4 1)) (Sheep [Point 3 2,Point 5 2,Point 3 4,Point 5 4])), Unrecognized))
--    SHEEP WINS
--    print(wolfMoveState (GameInfo (GameState (Wolf (Point 4 3)) (Sheep [Point 3 2,Point 5 2,Point 3 4,Point 5 4])), Unrecognized))
--    CHOOSE WOLF MOVE
    print(wolfMoveState (GameInfo (GameState (Wolf (Point 3 4)) (Sheep [Point 2 3 ,Point 4 2,Point 6 4,Point 4 5])) Unrecognized))

    print(validSheepMove (GameState (Wolf (Point 6 1)) (Sheep [Point 7 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1))