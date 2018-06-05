module Main where
import Test.HUnit as TTest
import Test.Framework
import Test.Framework.Providers.HUnit
import Model


sheepInitStates = [Point x y | y <- [0], x <- [0..7], odd x]
wolf = Wolf (Point 0 7)
g1 = GameState (Wolf (Point 1 1)) (Sheep sheepInitStates)

testHeroMove :: TTest.Test
testHeroMove = test [ "Task move sheep" ~: "0. Sheep: (1,0)"   ~: [Point 2 1, Point 0 1] ~=? (map (moveHero (Point 1 0)) sheepMoves),
                      "Task move sheep" ~: "1. Sheep: (3,0)"   ~: [Point 4 1, Point 2 1] ~=? (map (moveHero (Point 3 0)) sheepMoves),
                      "Task move sheep" ~: "2. Sheep: (5,0)"   ~: [Point 6 1, Point 4 1] ~=? (map (moveHero (Point 5 0)) sheepMoves),
                      "Task move sheep" ~: "3. Sheep: (7,0)"   ~: [Point 8 1, Point 6 1] ~=? (map (moveHero (Point 7 0)) sheepMoves)
                    ]

testValidateSheepMove :: TTest.Test
testValidateSheepMove = test [ "Validate move sheep" ~: "0.1 Wolf: (0,7); Sheep: (1,0) move: ( 1, 1) to Point 2 1"   ~: True   ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 1 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point   1  1)),
                               "Validate move sheep" ~: "0.2 Wolf: (0,7); Sheep: (1,0) move: (-1, 1) to Point 0 1"   ~: True   ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 1 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1)),
                               "Validate move sheep" ~: "1.1 Wolf: (0,7); Sheep: (3,0) move: ( 1, 1) to Point 4 1"   ~: True   ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 3 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point   1  1)),
                               "Validate move sheep" ~: "1.2 Wolf: (0,7); Sheep: (3,0) move: (-1, 1) to Point 2 1"   ~: True   ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 3 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1)),
                               "Validate move sheep" ~: "2.1 Wolf: (0,7); Sheep: (5,0) move: ( 1, 1) to Point 6 1"   ~: True   ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 5 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point   1  1)),
                               "Validate move sheep" ~: "2.2 Wolf: (0,7); Sheep: (5,0) move: (-1, 1) to Point 4 1"   ~: True   ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 5 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1)),
                               "Validate move sheep" ~: "3.1 Wolf: (0,7); Sheep: (7,0) move: ( 1, 1) to Point 8 1"   ~: False  ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 7 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point   1  1)),
                               "Validate move sheep" ~: "3.2 Wolf: (0,7); Sheep: (7,0) move: (-1, 1) to Point 6 1"   ~: True   ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 7 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1)),
                               "Validate move sheep" ~: "4.1 Wolf: (6,1); Sheep: (7,0) move: (-1, 1) to Point 6 1 - Conflict Woolf 1"
                                                                                                                     ~: False  ~=? (validSheepMove (GameState (Wolf (Point 6 1)) (Sheep [Point 7 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point (-1) 1)),
                               "Validate move sheep" ~: "4.2 Wolf: (2,1); Sheep: (1,0) move: (1, 1)  to Point 2 1 - Conflict Woolf 2"
                                                                                                                     ~: False  ~=? (validSheepMove (GameState (Wolf (Point 2 1)) (Sheep [Point 1 0,Point 3 0,Point 5 0,Point 7 0] )) 0 (Point   1  1)),
                               "Validate move sheep" ~: "5.1 Wolf: (0,7); Sheep: (5,0) move: (-1, 1) to Point 4 1 - Conflict with other Sheep in flock"
                                                                                                                     ~: False  ~=? (validSheepMove (GameState (Wolf (Point 0 7)) (Sheep [Point 5 0,Point 3 0,Point 4 1,Point 7 0] )) 0 (Point (-1) 1))
                               ]

testGetAcceptableMoves:: TTest.Test
testGetAcceptableMoves= test ["Test getSheepAcceptableMoves for sheep in position:" ~: "0. Wolf: (0,7); Sheep: (1,0), (3,0), (5,0), (7,0)" ~: [[Point 2 1, Point 0 1], [Point 4 1, Point 2 1], [Point 6 1, Point 4 1], [Point 6 1]]
                                                                                                                                                                                    ~=? (getSheepAcceptableMoves (GameState (Wolf (Point 0 7)) (Sheep [Point 1 0,Point 3 0,Point 5 0,Point 7 0] ))),
                            "Test getSheepAcceptableMoves for sheep in position:" ~: "1. Wolf: (0,7); Sheep: (5,0), (7,0)"                 ~: [[Point 6 1, Point 4 1], [Point 6 1]] ~=? (getSheepAcceptableMoves (GameState (Wolf (Point 0 7)) (Sheep [Point 5 0,Point 7 0] ))),
                            "Test getSheepAcceptableMoves for sheep in position:" ~: "2. Wolf: (0,7); Sheep: (7,0)"                        ~: [[Point 6 1]]                         ~=? (getSheepAcceptableMoves (GameState (Wolf (Point 0 7)) (Sheep [Point 7 0] ))),
                            "Test getSheepAcceptableMoves for sheep in position:" ~: "3. Wolf: (0,7); Sheep: (5,0)"                        ~: [[Point 6 1, Point 4 1]]              ~=? (getSheepAcceptableMoves (GameState (Wolf (Point 0 7)) (Sheep [Point 5 0] ))),
                            "Test getSheepAcceptableMoves for sheep in position:" ~: "4. Wolf: (6,1); Sheep: (7,0), (5,0)"                 ~: [[],[Point 4 1]]                      ~=? (getSheepAcceptableMoves (GameState (Wolf (Point 6 1)) (Sheep [Point 7 0, Point 5 0] ))),
                            "Test getSheepAcceptableMoves for sheep in position:" ~: "5. Wolf: (0,7); Sheep: (0,1), (1,0), (2,1), (7,0)"   ~: [[Point 1 2], [], [Point 3 2, Point 1 2], [Point 6 1]]
                                                                                                                                                                                    ~=? (getSheepAcceptableMoves (GameState (Wolf (Point 0 7)) (Sheep [Point 0 1,Point 1 0,Point 2 1,Point 7 0] )))
                           ]


testPointEqulity :: TTest.Test
testPointEqulity = test [ "Test point equality" ~: "1. Point (0,0) and Point (0,0)" ~: True  ~=? ((Point 0 0) == (Point 0 0)),
                          "Test point equality" ~: "2. Point (1,0) and Point (0,0)" ~: False ~=? ((Point 1 0) == (Point 0 0)),
                          "Test point equality" ~: "3. Point (1,1) and Point (1,1)" ~: True  ~=? ((Point 1 1) == (Point 1 1))
                        ]

main :: IO Counts
main = do _ <- runTestTT testHeroMove
          runTestTT testValidateSheepMove
          runTestTT testGetAcceptableMoves
          runTestTT testPointEqulity