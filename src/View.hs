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
initGameInfo = GameInfo initGameState Unrecognized
------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------
-----------------------   VIEWS   --------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
start :: GameInfo -> IO ()
start gameInfo = do
    putStrLn "Woolf \128058 and Sheep \128017 Game"
    putStrLn "1. Rozpoczęcie gry"
    putStrLn "2. Wczytanie gry"
    putStrLn "9. Zakończenie gry"
    option <- getLine
    if (checkOption ["1","2","9"] option) then case option of
                                                 "1" -> do print ("Rozpoczęcie gry" ++ option)
                                                           duringGame gameInfo
                                                 "2" -> print ("Wczytaj gre" ++ option)
                                                 "9" -> do putStrLn "Dziękujemy za grę. Do zobaczenia!"
                                                           return ()
      else
          do putStrLn "Błędnie Wybrana opcja\n"
             start initGameInfo
------------------------------------------------------------------------------------------------------------
checkOption:: [String] -> String -> Bool
checkOption [] _ = False
checkOption (x:xs) opt = if x == opt then True
                         else checkOption xs opt
------------------------------------------------------------------------------------------------------------
duringGame :: GameInfo -> IO ()
duringGame gameInfo = do
    printChessBoard (state gameInfo) 1 1
    putStrLn "Wciśnij przycisk aby wykonać jedną z następujących operacji"
    putStrLn "1. Wykonaj ruch"
    putStrLn "2. Zapisz grę"
    putStrLn "3. Zakończ rozgrywkę"
    option <- getLine
    if (checkOption ["1","2","3"] option) then case option of
                                                 "1" -> printPossibleMovements gameInfo
                                                 "2" -> actionAfterSavingState gameInfo
                                                 "3" -> start initGameInfo
      else
          do putStrLn "Błędnie Wybrana opcja\n"
             duringGame gameInfo

------------------------------------------------------------------------------------------------------------
printPossibleMovements gameInfo = do
   printProposalMove (state gameInfo)
   userMove <- getLine
   case reads userMove :: [(Int,String)] of
        [(n, a)] -> do print n
                       print (null a)
                       if (null a && validSelectedMove (state gameInfo) n) then do putStrLn "Send move to algorithm!"
                                                                                   putStrLn ""
                                                                                   duringGame gameInfo
                       else do putStrLn "Błędnie podana możliwość ruchu"
                               putStrLn ""
                               duringGame gameInfo
        _ -> do putStrLn "Błędnie podana możliwość ruchu"
                putStrLn ""
                duringGame gameInfo
------------------------------------------------------------------------------------------------------------
actionAfterSavingState gameInfo = do
    putStrLn "Zapisanie obecnego stanu gry"
    putStrLn "1. Powrót do gry"
    putStrLn "2. Zakończ rozgrywkę"
    option <- getLine
    if (checkOption ["1","2"] option) then case option of
                                            "1" -> duringGame gameInfo
                                            "2" -> start initGameInfo
      else
          do putStrLn "Błędnie Wybrana opcja\n"
             actionAfterSavingState gameInfo
------------------------------------------------------------------------------------------------------------
endGame = do
    putStrLn "Wygrał: "
    putStrLn "Aby powrócić do ekranu startowego wciśnij dowolny przycisk"
    x <- getChar
    start initGameInfo
------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
        start initGameInfo
