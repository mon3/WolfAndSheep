# WolfAndSheep

## API pomiędzy modułami
* Agregujący obiekt - dto : `GameInfo GameState Result`
* Metoda zwracająca początkowy stan całej gry:  `getFirstStep :: GameInfo`
* Metoda zwracająca stan gry po ruchu owiec: `updateGameInfo:: GameInfo -> GameInfo`