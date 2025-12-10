omid1Board :: GameBoard
omid1Board =
  [ [ sb11, sb12, sb13 ]
  , [ sb21, sb22, sb23 ]
  , [ sb31, sb32, sb33 ]
  ]
  where
    sb11 = UnFinished [[Empty,Full X,Empty],
                       [Full X,Empty,Full X],
                       [Full O,Full O,Full O]]

    sb12 = Finished (Win O)

    sb13 = UnFinished [[Empty,Full X,Empty],
                       [Empty,Full X,Empty],
                       [Empty,Full X,Empty]]

    sb21 = Finished (Win X)

    sb22 = UnFinished [[Empty,Full X,Empty],
                       [Empty,Full O,Empty],
                       [Empty,Full X,Empty]]

    sb23 = Finished (Win X)

    sb31 = Finished (Win O)

    sb32 = Finished Draw

    sb33 = UnFinished [[Empty,Full X,Empty],
                       [Empty,Full X,Empty],
                       [Empty,Empty,Empty]]

omid1Game :: Game
omid1Game = (omid1Board, O, (0,2))


dominatingX1 :: Game
dominatingX1 =
  ( board, X, (0,2))
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))

    winX = Finished (Win X)

    sb02 = UnFinished [[Full O, Empty,   Full X],
                       [Empty,  Empty,   Empty],
                       [Empty,  Full X,  Empty]]

    sb10 = UnFinished [[Empty, Full X,  Empty],
                       [Full O, Empty,  Empty],
                       [Empty, Empty,   Empty]]

    sb11 = UnFinished [[Empty, Empty, Full X],
                       [Empty, Full O, Empty],
                       [Empty, Empty,  Empty]]

    sb21 = UnFinished [[Empty, Full X, Empty],
                       [Empty, Empty,  Full O],
                       [Empty, Empty,  Empty]]

    board =
      [ [winX, emptySB, sb02]
      , [sb10, winX, sb11]
      , [emptySB, sb21, emptySB]
      ]

dominatingO1 :: Game
dominatingO1 =
  ( board, O, (2,0) )
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))

    winO = Finished (Win O)

    sb20 = UnFinished [[Empty,   Full O, Empty],
                       [Empty,   Empty,  Empty],
                       [Full X,  Empty,  Empty]]

    sb01 = UnFinished [[Full O, Empty,   Empty],
                       [Empty,  Empty,   Empty],
                       [Empty,  Full X,  Empty]]

    sb12 = UnFinished [[Empty, Full O, Empty],
                       [Empty, Empty,  Empty],
                       [Empty, Empty,  Full X]]

    sb02 = UnFinished [[Empty, Full O,  Full X],
                       [Empty, Empty,   Empty],
                       [Empty, Empty,   Empty]]

    sb21 = UnFinished [[Empty,  Empty,  Empty],
                       [Full O, Empty,  Empty],
                       [Empty,  Empty,  Full X]]

    board =
      [ [emptySB, winO, sb02]
      , [sb01, emptySB, sb12]
      , [sb20, sb21, winO]
      ]
