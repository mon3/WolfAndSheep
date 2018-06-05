module Controller where
import Model

endLine = "\n"

printProposalMove ::GameState -> IO ()
printProposalMove gameState = putStrLn (allPossibleMovesForEverySheep (getSheepAcceptableMoves gameState) 0 ("Współrzędne możliwych do wykonania ruchów (x,y)" ++ endLine))


allPossibleMovesForEverySheep :: [[Point]] -> Int -> String -> String
allPossibleMovesForEverySheep moves idx writing | idx < length moves = writing ++ (allPossibleMovesForEverySheep moves (idx+1) (printSingleSheep idx (moves !! idx)))
                                                | otherwise = writing ++ endLine


printSingleSheep :: Int-> [Point] -> String
printSingleSheep idx list | idx < 0 || idx > 3 = error "Błędny indeks"
                          | otherwise          = "Dla owcy nr." ++ (show idx) ++ endLine ++ (getOption idx list True)++ endLine
                                                 where getOption id []    _     = "Brak ruchu" ++ endLine
                                                       getOption id [x]   True  = (show (2*id)) ++ ". " ++ (show (xPoint x)) ++ " " ++ (show (yPoint x))
                                                       getOption id [x]   False = (show (2*id + 1)) ++ ". " ++ (show (xPoint x)) ++ " " ++ (show (yPoint x)) ++endLine
                                                       getOption id [x,y] _     = (getOption id [x] True) ++ endLine ++ (getOption id [y] False)
                                                       getOption _  _     _     = error "Błędne możliwe ruchy"