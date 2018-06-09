module View where
import Model
import GUI
import Controller
import System.Directory
import System.IO
import System.IO.Error
import Data.Typeable
import Control.Exception
import Text.Read


saveStateLocation = "./resources/"
fileExtensionOfSaveState = ".txt"
------------------------------------------------------------------------------------------------------------
-----------------------   VIEWS   --------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
start :: IO ()
start = do
    putStrLn "Wolf \128058 and Sheep \128017 Game"
    putStrLn "1. Rozpoczęcie gry"
    putStrLn "2. Wczytanie gry"
    putStrLn "9. Zakończenie gry"
    option <- getLine
    if (checkOption ["1","2","9"] option) then case option of
                                                 "1" -> do print ("Rozpoczęcie gry" ++ option)
                                                           duringGame getFirstGameStep
                                                 "2" -> loadGame
                                                 "9" -> do putStrLn "Dziękujemy za grę. Do zobaczenia!"
                                                           return ()
      else
          do putStrLn "Błędnie Wybrana opcja\n"
             start
------------------------------------------------------------------------------------------------------------
checkOption:: [String] -> String -> Bool
checkOption [] _ = False
checkOption (x:xs) opt = if x == opt then True
                         else checkOption xs opt
------------------------------------------------------------------------------------------------------------
duringGame :: GameInfo -> IO ()
duringGame gameInfo = do
    if ((result gameInfo) /= Unrecognized) then endGame gameInfo
        else do
             printChessBoard (state gameInfo) 1 1
             putStrLn "Wciśnij przycisk aby wykonać jedną z następujących operacji"
             putStrLn "1. Wykonaj ruch"
             putStrLn "2. Zapisz grę"
             putStrLn "3. Zakończ rozgrywkę"
             option <- getLine
             if (checkOption ["1","2","3"] option) then case option of
                                                          "1" -> printPossibleMovements gameInfo
                                                          "2" -> actionAfterSavingState gameInfo
                                                          "3" -> start
               else
                   do putStrLn "Błędnie Wybrana opcja\n"
                      duringGame gameInfo

------------------------------------------------------------------------------------------------------------
printPossibleMovements gameInfo = do
   printProposalMove (state gameInfo)
   userMove <- getLine
   case reads userMove :: [(Int,String)] of
        [(n, a)] -> do
                       if (null a && validSelectedMove (state gameInfo) n) then do putStrLn " "
                                                                                   duringGame (wolfMoveState (getGameInfoAfterSheepMove gameInfo n))
                       else do putStrLn "Błędnie podana możliwość ruchu"
                               putStrLn ""
                               duringGame gameInfo
        _ -> do putStrLn "Błędnie podana możliwość ruchu"
                putStrLn ""
                duringGame gameInfo
------------------------------------------------------------------------------------------------------------
loadGame =
  catch (do putStrLn "Wybierz jedną z poniższych gier:"
            files <- listDirectory saveStateLocation
            putStrLn "0. Powrót do menu głównego"
            putStr (unlines [show (fst x) ++ ". " ++ (snd x) | x <- (zip [1 .. (length files)] files)])
            option <- getLine
            if (checkOption [ show x | x <- [0 .. (length files) ]] option) then case option of
                                                                                   "0" -> start
                                                                                   _ -> checkLoadedGame (files !! ((read option::Int) - 1))
            else loadGameErrorReaction "Błędnie Wybrana opcja\n"
        ) errorHandler
        where errorHandler e = if isDoesNotExistError e then loadGameErrorReaction ("Podany plik nie istnieje")
                                else if isPermissionError e then loadGameErrorReaction ("Brak dostępu")
                                        else responseOnError
                                             where responseOnError = loadGameErrorReaction "Wystąpił nieznany problem"

------------------------------------------------------------------------------------------------------------
checkLoadedGame:: String -> IO ()
checkLoadedGame fileName =
    catch (do putStrLn ("Wybrano plik: " ++ fileName)
              gs <- openFile (saveStateLocation ++ fileName ) ReadMode
              contents <- hGetContents gs
              if ((readMaybe contents :: Maybe GameInfo) == Nothing) then loadGameErrorReaction "Wystąpił problem z wybranym plikiem."
               else if( (Just(read contents::GameInfo) /= Nothing) && (result (read contents::GameInfo) == Unrecognized)) then do putStrLn ""
                                                                                                                                  duringGame (read contents::GameInfo)
                     else if (Just(read contents::GameInfo) == Nothing) then loadGameErrorReaction "Wystąpił problem z wybranym plikiem."
                           else endGame ((read contents::GameInfo))
          ) errorHandler
          where errorHandler e = if isDoesNotExistError e then loadGameErrorReaction "Podany plik nie istnieje"
                                  else if isPermissionError e then loadGameErrorReaction "Brak dostępu"
                                          else responseOnError
                                               where responseOnError = loadGameErrorReaction "Wystąpił nieznany problem"
------------------------------------------------------------------------------------------------------------
actionAfterSavingState gameInfo = do
    putStrLn "Zapisanie obecnego stanu gry"
    putStrLn "Podaj nazwę pliku. Wszystkie stany są zapisane w folderze resources"
    fileName <- getLine
    isExist <- doesFileExist (saveStateLocation ++ fileName ++ fileExtensionOfSaveState)
    if isExist then do putStrLn "Plik o podanej nazwie już istnieje. Podaj inną"
                       actionAfterSavingState gameInfo
    else writeFile (saveStateLocation ++ fileName ++ fileExtensionOfSaveState) (show gameInfo)
    putStrLn "1. Powrót do gry"
    putStrLn "2. Zakończ rozgrywkę"
    option <- getLine
    if (checkOption ["1","2"] option) then case option of
                                            "1" -> duringGame gameInfo
                                            "2" -> start
      else
          do putStrLn "Błędnie Wybrana opcja - Powrót do menu głównego"
             putStrLn ""
             start
------------------------------------------------------------------------------------------------------------
loadGameErrorReaction :: String -> IO ()
loadGameErrorReaction str = do putStrLn str
                               putStrLn ""
                               loadGame
------------------------------------------------------------------------------------------------------------
endGame gameInfo = do
    putStrLn ("Wygrał: " ++ (show (result gameInfo)))
    putStrLn "Aby powrócić do ekranu startowego wciśnij dowolny przycisk"
    x <- getChar
    start
------------------------------------------------------------------------------------------------------------

