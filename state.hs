data PointType = Point Int Int deriving (Eq,Show)

data Wolf = Wolf PointType deriving (Eq,Show)
data Sheep = Sheep PointType PointType PointType PointType deriving (Eq,Show)
data GameState = State Wolf Sheep deriving (Eq,Show)


wolf = Wolf (Point 1 1) 

sheep = Sheep (Point 8 2) (Point 8 4) (Point 8 6) (Point 8 8)

gameState = State wolf sheep
-- t :: Tree Int
-- t = Node 5 (Node 3 (Node 8 Empty Empty)(Node 1 Empty Empty)) (Node 4 Empty(Node 6 Empty Empty))
main = print(gameState)