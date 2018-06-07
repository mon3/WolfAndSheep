module Main where
import Model
import AI
import System.Random
import System.IO.Unsafe


atRandIndex :: [Point] -> IO Point  -- note that this is gives itself an IO action
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return (l !! i)



initialize :: [Point] -> [Point] -> IO GameState
initialize wolfStates sheepStates = do
    wolfState <- (atRandIndex wolfStates)
    let game = GameState (Wolf wolfState) (Sheep sheepStates)
    return game


-- assuming wolf always does the first step
getFirstGameStep :: [Point] -> [Point] -> GameDTO
getFirstGameStep wolfStates sheepStates = let gsFirst = initialize wolfStates sheepStates
                                              in wolfMoveState (GameDTO (unsafePerformIO gsFirst, Unrecognized))


---------------------------INITIALIZATION---------------------------------------------
wolfInitStates = [Point x y | y <- [7], x <- [0..7], odd x]
sheepInitStates = [Point x y | y <- [0], x <- [0..7], even x]
sheep = Sheep sheepInitStates

-- to test if wolf move is valid
main = do
    gameState <- (initialize wolfInitStates sheepInitStates)
    print(gameState)
    print(getHeuristic (GameState (Wolf (Point 3 5)) (Sheep [Point 2 4,Point 4 4,Point 2 6,Point 4 6])))

    print(validSheepMove gameState 0 (Point (-1) 1))
    print(g 3)
--    let val = (getHeuristic gameState)
    let newPos =[ moveHero (getWolfMove gameState) point | point <- wolfMoves, (validWolfMove gameState point)]
    let res =  wolfWon (GameState (Wolf (Point 7 1)) (Sheep [Point 4 4,Point 2 4,Point 0 4,Point 0 6])) (Point (-1) (-1))
    print( (possibleNewSheepPositions (GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6]))))
    print(getSheepNextStates (GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6])))
    print(bestMove 0 ((GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6])),1000) (getWolfNextMoves (GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6]))))
    print(GameDTO (gameState, Unrecognized))
    print(validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 5 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1))
    print([getHeuristic (GameState (Wolf (Point 4 4)) sheep) | sheep <- (possibleNewSheepPositions (GameState (Wolf (Point 4 4)) (Sheep [Point 3 3,Point 3 5,Point 6 2,Point 6 6])))])
    let newState = moveSheep gameState 0 (Point (1) 1)
    print( [minmaxWolfMove 2 newState wolfVector | wolfVector <- wolfMoves, validWolfMove newState wolfVector])
    print(bestWolfMoveValue [minmaxWolfMove 2 newState wolfVector | wolfVector <- wolfMoves, validWolfMove newState wolfVector])
--    WOLF WINS
--    print(wolfMoveState (GameDTO((GameState (Wolf (Point 4 1)) (Sheep [Point 3 2,Point 5 2,Point 3 4,Point 5 4])), Unrecognized)))
--    SHEEP WINS
--    print(wolfMoveState (GameDTO((GameState (Wolf (Point 4 3)) (Sheep [Point 3 2,Point 5 2,Point 3 4,Point 5 4])), Unrecognized)))
--    CHOOSE WOLF MOVE
    print(wolfMoveState (GameDTO((GameState (Wolf (Point 3 4)) (Sheep [Point 2 3 ,Point 4 2,Point 6 4,Point 4 5])), Unrecognized)))

    print(validSheepMove (GameState (Wolf (Point 6 1)) (Sheep [Point 7 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1))
    print(getFirstGameStep wolfInitStates sheepInitStates)