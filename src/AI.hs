module AI where

import Model


g:: Int -> String
g _ = "Success!"

data GameTree a = Empty | Node a [GameTree a] deriving (Show)

data ScoreTree a = GameTree a

depth :: GameTree a -> Int
depth Empty = 0
depth (Node _ []) = 1
depth (Node _ ts) = 1 + maximum(map depth ts)

-- sprawdza czy wilk wygral albo wygra po wykonaniu danego kroku (wybranego)
wolfWon :: GameState -> Point -> Bool
wolfWon (GameState (Wolf (Point _ 0)) sheep) _ = True
wolfWon gs dpoint = if (validWolfMove gs dpoint) && (yPoint newPos == 0) then True else False
                    where newPos = getWolfMove (moveWolf gs dpoint)


-- generuje nastepne mozliwe ruchy wilka ze wzgledu na dany stan gry
getWolfNextMoves :: GameState -> [GameState]
getWolfNextMoves (GameState (Wolf wolf) sheep) = [ GameState (Wolf (moveHero wolf point)) sheep | point <- wolfMoves, (validWolfMove (GameState (Wolf wolf) sheep) point)]


-- sprawdza, czy ostatni krok wilka bedzie zwycieski (jesli wybierze 1 z 4 dozwolonycb kierunkow)
lastWolfMove :: GameState -> Bool
lastWolfMove gs = if elem True resList then True
                  else False
                    where resList = map (wolfWon gs) x
                          x = [a | a<-wolfMoves]



buildGameTree :: GameState -> GameTree GameState
buildGameTree gs  | (lastWolfMove gs)  = Node gs []
                  | otherwise = Node gs [(buildGameTree state) | state <- (getWolfNextMoves gs), (yPoint(getWolfMove gs) >= yPoint(getWolfMove state)) ]
--                  | otherwise = Node gs [(buildGameTree state) | state <- (getWolfNextMoves gs) ]

--                  (map (fmap buildGameTree) childWolfMoves)
--                    where
--                      possibleMoves = getWolfNextMoves gs
--                  childWolfMoves =  [(moveWolf a ) | a <- possibleMoves]
--                  map(\move -> moveWolf gs move) possibleMoves


prune :: Int -> GameTree a -> GameTree a
prune 0 (Node a _) = Node a []
prune d (Node a xs) = Node a (map (prune (d - 1)) xs)


--wolfPaths :: GameState -> [[Point]]
--wolfPaths (GameState (Wolf (Point _ 0)) sheep)  = [[]]
--wolfPaths gs = map


--getHeuristic :: GameState -> Int
--getHeuristic (GameState (Wolf (Point _ 0)) sheep) = 0
--getHeuristic (GameState (Wolf position) sheep) = map validMove (newPos boardXY)
--                                                 where newPos = [ newPosition point | point <- wolfMoves, (validWolfMove (newPosition point) boardXY)]
--                                                       newPosition = moveHero position

-- heurystyka oceniająca wartość danego układu planszy dla wilka

-- uruchomienie algorytmu wyboru ruchu wilka
--wolfMoveState :: GameState -> (Result, GameState)
--wolfMoveState gs =
--	if (null possibleMoves)
--	then
--	 (SheepWins, gs)
--	else
--	 if (lastWolfMove gs)
--	 then
--	  (WolfWins, gs)
--	 else
--	  (Unrecognized, wolfMove gs)
--	where
--	 possibleMoves = [vector | vector <- possibleWolfMoves, canWolfMove state vector]
