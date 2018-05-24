module Main where
import Model
import AI
import System.Random


atRandIndex :: [Point] -> IO Point  -- note that this is gives itself an IO action
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return (l !! i)



initialize :: [Point] -> [Point] -> IO GameState
initialize wolfStates sheepStates = do
    wolfState <- (atRandIndex wolfStates)
    let game = GameState (Wolf wolfState) (Sheep sheepStates)
    return game



---------------------------INITIALIZATION---------------------------------------------
wolfInitStates = [Point x y | y <- [7], x <- [0..7], odd x]
sheepInitStates = [Point x y | y <- [0], x <- [0..7], even x]
sheep = Sheep sheepInitStates

-- to test if wolf move is valid
main = do
    gameState <- (initialize wolfInitStates sheepInitStates)
    print(gameState)
    print(validSheepMove gameState 0 (Point (-1) 1))
    print(g 3)
--    let val = (getHeuristic gameState)
    let newPos =[ moveHero (getWolfMove gameState) point | point <- wolfMoves, (validWolfMove gameState point)]
    let res =  wolfWon (GameState (Wolf (Point 7 1)) (Sheep [Point 4 4,Point 2 4,Point 0 4,Point 0 6])) (Point (-1) (-1))
    let gameTree = buildGameTree (GameState (Wolf (Point 2 3)) (Sheep [Point 7 2,Point 4 3,Point 2 1,Point 0 1]))
    print(gameTree)
    print(depth gameTree)
--    print(prune 2 gameTree)
--    print(lastWolfMove (GameState (Wolf (Point 6 2)) (Sheep [Point 4 4,Point 2 4,Point 0 4,Point 0 6])))

--    print(buildGameTree (GameState (Wolf (Point 1 2)) (Sheep [Point 7 2,Point 4 3,Point 2 1,Point 0 1])))




-- main = print(initialize wolfInitStates sheepInitStates)
-- gameState = (initialize wolfInitStates sheepInitStates)
-- main = print(moveWolf gameState (Point 1 (-1)))
-- main = print(moveSheep gameState 3 (Point 1 (1))) -- owce o indeksach [0;3] poruszaja sie po planszy
-- main = print(map comparePositions [(Point 1 1) (Point 2 2)] (Point 1 1))

-- main = print(sheep)
-- main = print(validWolfMove gameState (Point 1 8))

-- main = print(validMove (Point 1 8) boardXY)

-- main = do
--     res <- (atRandIndex wolfInitStates)
--     print(res)
    -- x <- res   --strzałka wyciąga wartość z monady do x
    -- print(x)