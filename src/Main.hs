module Main where
import Model
import AI
import Controller
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



-- to test if wolf move is valid
main = do
    gameState <- (initialize wolfInitStates sheepInitStates)
    print(gameState)
    print(validSheepMove gameState 0 (Point (-1) 1))
    print (g 3)
    print getFirstStep




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