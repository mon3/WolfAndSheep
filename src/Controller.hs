module Controller where
import Model

endLine = "\n"

printProposalMove ::GameState -> IO ()
printProposalMove gameState = putStrLn (allPossibleMovesForEverySheep (getSheepAcceptableMoves gameState) 0 ("Współrzędne możliwych do wykonania ruchów (x,y)" ++ endLine))


allPossibleMovesForEverySheep :: [[Point]] -> Int -> String -> String
allPossibleMovesForEverySheep moves idx writing | idx < length moves = writing ++ (allPossibleMovesForEverySheep moves (idx+1) (printSingleSheep idx sheepProposedMoves))
                                                | otherwise = writing ++ endLine
                                                  where sheepProposedMoves = moves !! idx



printSingleSheep :: Int-> [Point] -> String
printSingleSheep idx list | idx < 0 || idx > 3 = error "Błędny indeks"
                          | otherwise          = "Dla owcy nr." ++ (show (idx+1)) ++ endLine ++ (getOption idx list True)++ endLine
                                                 where getOption id []    _     = "Brak ruchu" ++ endLine
                                                       getOption id [x]   True  = (show (2*id)) ++ ". " ++ (show (xPoint x)) ++ " " ++ (show (yPoint x))
                                                       getOption id [x]   False = (show (2*id + 1)) ++ ". " ++ (show (xPoint x)) ++ " " ++ (show (yPoint x)) ++endLine
                                                       getOption id [x,y] _     = (getOption id [x] True) ++ endLine ++ (getOption id [y] False)
                                                       getOption _  _     _     = error "Błędne możliwe ruchy"


validSelectedMove  :: GameState-> Int -> Bool
validSelectedMove g selectedRecordNumber = elem selectedRecordNumber (getRecordNumberList g)

getRecordNumberList :: GameState -> [Int]
getRecordNumberList g=  getSheepRecordNumber (getSheepAcceptableMoves g) 0 []


getSheepRecordNumber :: [[Point]] -> Int -> [Int] -> [Int]
getSheepRecordNumber moves idx list | idx < length moves = getSheepRecordNumber moves (idx + 1) ( list ++ (getIndexes idx (moves !! idx)))
                                    | otherwise          = list
                                                           where getIndexes idx list | length list == 0 = []
                                                                                     | length list == 1 = [2*idx]
                                                                                     | length list == 2 = [2*idx, 2*idx +1]
                                                                                     | otherwise = []

