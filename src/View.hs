--module View where
import Model
import GUI
import Controller
------------------------------------------------------------------------------------------------------------
-----------------------   START   --------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
sheepInitStates = [Point x y | y <- [0], x <- [0..7], odd x]
wolf = Wolf (Point 0 7)
sheep = Sheep sheepInitStates
initGameState = GameState wolf sheep
------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------
-----------------------   VIEWS   --------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
start :: GameState -> IO ()
start state = do
    putStrLn "Woolf \128058 and Sheep \128017 Game"
    putStrLn "1. Rozpoczęcie gry"
    putStrLn "2. Wczytanie gry"
    putStrLn "9. Zakończenie gry"
    option <- getLine
    if (checkOption ["1","2","9"] option) then case option of
                                                 "1" -> do print ("Rozpoczęcie gry" ++ option)
                                                           duringGame state
                                                 "2" -> print ("Wczytaj gre" ++ option)
                                                 "9" -> do putStrLn "Dziękujemy za grę. Do zobaczenia!"
                                                           return ()
      else
          do putStrLn "Błędnie Wybrana opcja\n"
             start initGameState
------------------------------------------------------------------------------------------------------------
checkOption:: [String] -> String -> Bool
checkOption [] _ = False
checkOption (x:xs) opt = if x == opt then True
                         else checkOption xs opt
------------------------------------------------------------------------------------------------------------
duringGame :: GameState -> IO ()
duringGame state = do
    printChessBoard state 1 1
    putStrLn "Wciśnij przycisk aby wykonać jedną z następujących operacji"
    putStrLn "1. Wykonaj ruch"
    putStrLn "2. Zapisz grę"
    putStrLn "3. Zakończ rozgrywkę"
    option <- getLine
    if (checkOption ["1","2","3"] option) then case option of
                                                 "1" -> printProposalMove state
                                                 "2" -> actionAfterSavingState state
                                                 "3" -> start initGameState
      else
          do putStrLn "Błędnie Wybrana opcja\n"
             duringGame state

------------------------------------------------------------------------------------------------------------
actionAfterSavingState state = do
    putStrLn "Zapisanie obecnego stanu gry"
    putStrLn "1. Powrót do gry"
    putStrLn "2. Zakończ rozgrywkę"
    option <- getLine
    if (checkOption ["1","2"] option) then case option of
                                            "1" -> duringGame state
                                            "2" -> start initGameState
      else
          do putStrLn "Błędnie Wybrana opcja\n"
             actionAfterSavingState initGameState
------------------------------------------------------------------------------------------------------------
endGame = do
    putStrLn "Wygrał: "
    putStrLn "Aby powrócić do ekranu startowego wciśnij dowolny przycisk"
    x <- getChar
    start initGameState
------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
        start initGameState
