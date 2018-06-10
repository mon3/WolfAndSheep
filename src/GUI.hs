module GUI where
import Model
import System.Console.ANSI

block = "  "

printChessBoard:: GameState -> Int -> Int -> IO ()
printChessBoard gameState idxCol idxRow = printChess 64 0 0 gameState idxCol idxRow
                                          where printChess size nrCol nrRow (GameState (Wolf wolf) sheep) col row | mod (nrCol + nrRow) 2 == 0 && mod size 8 /= 1 && size > 0 = do pickToPrint 0 nrCol nrRow wolf sheep col row
                                                                                                                                                                                   printChess (size - 1) (nrCol + 1) nrRow gameState (col + 6) 4
                                                                                                                  | mod (nrCol + nrRow) 2 /= 0 && mod size 8 /= 1 && size > 0 = do pickToPrint 1 nrCol nrRow wolf sheep col row
                                                                                                                                                                                   printChess (size - 1) (nrCol + 1) nrRow gameState (col + 6) 4
                                                                                                                  | mod (nrCol + nrRow) 2 /= 0 && mod size 8 == 1 && size > 0 = do pickToPrint 1 nrCol nrRow wolf sheep col row
                                                                                                                                                                                   putStrLn ""
                                                                                                                                                                                   printChess (size - 1) 0 (nrRow + 1) gameState 1 2
                                                                                                                  | mod (nrCol + nrRow) 2 == 0 && mod size 8 == 1 && size > 0 = do pickToPrint 0 nrCol nrRow wolf sheep col row
                                                                                                                                                                                   putStrLn ""
                                                                                                                                                                                   printChess (size - 1) 0 (nrRow + 1) gameState 1 2
                                                                                                                  | otherwise = putStrLn " "
                                                                                                                  where pickToPrint color nrCol nrRow (Point wCol wRow) (Sheep sheepTiles) col row | wCol == nrCol && wRow == nrRow             = do printWolfSquare col row
                                                                                                                                                                                                   | isSheepTile sheepTiles (Point nrCol nrRow) = do printSheepSquare col row
                                                                                                                                                                                                   | color == 0                                 = do printWhiteEmptySquare col row
                                                                                                                                                                                                   | otherwise                                  = do printBlackEmptySquare col row
                                                                                                                                                                                                   where isSheepTile sheep (Point nrCol nrRow) = length (filter (True ==) ( map (== (Point nrCol nrRow)) sheep)) /= 0



printWhiteEmptySquare :: Int -> Int ->  IO ()
printWhiteEmptySquare col row = printColorSquare Vivid Red Dull Black "" col row

printBlackEmptySquare :: Int -> Int -> IO ()
printBlackEmptySquare col row = printColorSquare Vivid Yellow Vivid White "" col row

printSheepSquare :: Int -> Int -> IO ()
printSheepSquare col row =  printColorSquare Dull Blue Vivid White "\128017 " col row
--printSheepSquare col row =  printColorSquare Dull Green Vivid White "S " col row

printWolfSquare :: Int -> Int -> IO ()
printWolfSquare col row =  printColorSquare Vivid Red Vivid White "\128058 " col row
--printWolfSquare col row =  printColorSquare Vivid Red Vivid White "W " col row


printColorSquare:: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> Int -> Int -> IO ()
printColorSquare fgi fg bgi bg str column row | (length str) == 0 = printSquare "  " column row False
                                              | otherwise       = printSquare str column row True
                                              where printSquare str column row bool= do
                                                                         cursorUpLine row
                                                                         cursorForward column
                                                                         setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
                                                                         putStr (block ++ block ++ block ) -- ++ "\9145")
                                                                         setSGR [ ]
                                                                         putStrLn ""
                                                                         cursorForward column
                                                                         setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
                                                                         putStr (block ++ str ++ block ) -- ++ "\9145")
                                                                         setSGR [ ]
                                                                         putStrLn ""
                                                                         cursorForward column
                                                                         setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
                                                                         if bool then setSGR [SetConsoleIntensity BoldIntensity,SetUnderlining SingleUnderline]
                                                                         else setSGR [SetUnderlining NoUnderline]
                                                                         putStr (block ++ block ++ block ) -- ++ "\9145")
                                                                         setSGR [ ]
                                                                         putStrLn ""
                                                                         putStrLn ""


