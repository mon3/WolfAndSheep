module Controller where
import Model
import AI
import Data.Maybe
import Data.List
import System.IO.Unsafe
import System.Random

endLine = "\n"
------------------------------------------------------------------------------------------------------------
printProposalMove ::GameState -> IO ()
printProposalMove gameState = putStrLn (allPossibleMovesForEverySheep (getSheepAcceptableMoves gameState) 0 ("Współrzędne możliwych do wykonania ruchów (x,y)" ++ endLine))

------------------------------------------------------------------------------------------------------------
allPossibleMovesForEverySheep :: [[Point]] -> Int -> String -> String
allPossibleMovesForEverySheep moves idx writing | idx < length moves = writing ++ (allPossibleMovesForEverySheep moves (idx+1) (printSingleSheep idx sheepProposedMoves))
                                                | otherwise = writing ++ endLine
                                                  where sheepProposedMoves = moves !! idx


------------------------------------------------------------------------------------------------------------
printSingleSheep :: Int-> [Point] -> String
printSingleSheep idx list | idx < 0 || idx > 3 = error "Błędny indeks"
                          | otherwise          = "Dla owcy nr." ++ (show (idx+1)) ++ endLine ++ (getOption idx list True)++ endLine
                                                 where getOption id []    _     = "Brak ruchu" ++ endLine
                                                       getOption id [x]   True  = (show (2*id)) ++ ". " ++ (show (xPoint x)) ++ " " ++ (show (yPoint x)) ++ endLine
                                                       getOption id [x]   False = (show (2*id + 1)) ++ ". " ++ (show (xPoint x)) ++ " " ++ (show (yPoint x)) ++ endLine
                                                       getOption id [x,y] _     = (getOption id [x] True) ++ (getOption id [y] False)
                                                       getOption _  _     _     = error "Błędne możliwe ruchy"


------------------------------------------------------------------------------------------------------------
validSelectedMove  :: GameState-> Int -> Bool
validSelectedMove g selectedRecordNumber = elem selectedRecordNumber (getRecordNumberList g)

------------------------------------------------------------------------------------------------------------
getRecordNumberList :: GameState -> [Int]
getRecordNumberList g =  getSheepRecordNumber (getSheepAcceptableMoves g) 0 []

------------------------------------------------------------------------------------------------------------
getSheepRecordNumber :: [[Point]] -> Int -> [Int] -> [Int]
getSheepRecordNumber moves idx list | idx < length moves = getSheepRecordNumber moves (idx + 1) ( list ++ (getIndexes idx (moves !! idx)))
                                    | otherwise          = list
                                                           where getIndexes idx list | length list == 0 = []
                                                                                     | length list == 1 = [2*idx]
                                                                                     | length list == 2 = [2*idx, 2*idx +1]
                                                                                     | otherwise = []

------------------------------------------------------------------------------------------------------------
getGameInfoAfterSheepMove :: GameInfo -> Int -> GameInfo
getGameInfoAfterSheepMove (GameInfo (GameState wolf (Sheep sheep)) res) idx = GameInfo (GameState (wolf) (Sheep (map (\x -> if ((fromJust $ elemIndex x sheep) == (div idx 2)) then np else x) sheep))) res
                                                                              where np = ((getSheepAcceptableMoves (GameState wolf (Sheep sheep))) !! (div idx 2)) !! (mod idx 2)

------------------------------------------------------------------------------------------------------------
-- start of MIN-MAX algorithm which selects best wolf move
-- return GameInfo (new gamestate) (game result)
wolfMoveState :: GameInfo -> GameInfo
wolfMoveState (GameInfo gs res) =
  if (null possibleMoves)
  then
   GameInfo gs SheepWins
  else
   if (lastWolfMove gs)
   then
    GameInfo gs WolfWins
   else
    GameInfo (wolfGameMove gs) Unrecognized
  where
   possibleMoves = [displacement | displacement <- wolfMoves, validWolfMove gs displacement]

------------------------------------------------------------------------------------------------------------
atRandIndex :: [Point] -> IO Point  -- note that this is gives itself an IO action
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return (l !! i)

------------------------------------------------------------------------------------------------------------
initialize :: [Point] -> [Point] -> IO GameState
initialize wolfStates sheepStates = do
    wolfState <- (atRandIndex wolfStates)
    let game = GameState (Wolf wolfState) (Sheep sheepStates)
    return game

------------------------------------------------------------------------------------------------------------
-- assuming wolf always does the first step
getFirstGameStep ::  GameInfo
getFirstGameStep  = wolfMoveState (GameInfo (unsafePerformIO (initialize wolfInitStates sheepInitStates)) Unrecognized)

------------------------------------------------------------------------------------------------------------
