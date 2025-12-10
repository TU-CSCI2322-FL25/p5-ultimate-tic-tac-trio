chaoticGame1 :: Game
chaoticGame1 =
  ( board, X, (0,2) )
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))
    board =
      [ [UnFinished [[Full O, Empty, Full X],
                     [Empty, Full O, Empty],
                     [Full X, Empty, Empty]],
         UnFinished [[Empty, Full X, Empty],
                     [Full O, Empty, Full X],
                     [Empty, Empty, Full O]],
         UnFinished [[Full O, Full X, Empty],
                     [Empty, Full O, Full X],
                     [Full O, Empty, Empty]]]
      , [UnFinished [[Empty, Full O, Empty],
                     [Full X, Empty, Full O],
                     [Empty, Full X, Empty]],
         UnFinished [[Full X, Empty, Full O],
                     [Empty, Full X, Empty],
                     [Full O, Empty, Full X]],
         UnFinished [[Empty, Full X, Full O],
                     [Full X, Empty, Full O],
                     [Empty, Full X, Empty]]]
      , [UnFinished [[Full O, Empty, Full X],
                     [Empty, Full O, Full X],
                     [Full X, Empty, Empty]],
         UnFinished [[Empty, Full X, Empty],
                     [Full O, Empty, Full X],
                     [Full O, Empty, Full X]],
         UnFinished [[Empty, Empty, Full O],
                     [Full X, Full O, Empty],
                     [Full X, Empty, Full O]]]
      ]

chaoticGame2 :: Game
chaoticGame2 =
  ( board, O, (1,0))
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))
    board =
      [ [UnFinished [[Full X, Empty, Full O],
                     [Empty, Full X, Full O],
                     [Full O, Empty, Empty]],
         UnFinished [[Empty, Full O, Empty],
                     [Full X, Empty, Full O],
                     [Empty, Full X, Empty]],
         UnFinished [[Full O, Full X, Empty],
                     [Empty, Full O, Full X],
                     [Empty, Empty, Full O]]]
      , [UnFinished [[Full X, Empty, Full O],
                     [Full O, Empty, Full X],
                     [Empty, Full O, Empty]],
         UnFinished [[Empty, Full X, Full O],
                     [Full X, Empty, Full O],
                     [Full O, Full X, Empty]],
         UnFinished [[Full O, Empty, Full X],
                     [Empty, Full X, Full O],
                     [Full X, Empty, Empty]]]
      , [UnFinished [[Empty, Full O, Empty],
                     [Full X, Full O, Empty],
                     [Full X, Empty, Full O]],
         UnFinished [[Full O, Empty, Full X],
                     [Empty, Full O, Full X],
                     [Empty, Full X, Full O]],
         UnFinished [[Full X, Full O, Empty],
                     [Full O, Empty, Full X],
                     [Empty, Full O, Empty]]]
      ]

chaoticGame3 :: Game
chaoticGame3 =
  ( board, X, (2,1))
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))
    board =
      [ [UnFinished [[Full O, Full X, Empty],
                     [Empty, Full O, Full X],
                     [Full X, Empty, Full O]],
         UnFinished [[Empty, Full X, Full O],
                     [Full O, Empty, Full X],
                     [Full X, Full O, Empty]],
         UnFinished [[Full O, Empty, Full X],
                     [Empty, Full X, Full O],
                     [Full O, Empty, Full X]]]
      , [UnFinished [[Empty, Full O, Full X],
                     [Full X, Empty, Full O],
                     [Empty, Full X, Full O]],
         UnFinished [[Full X, Empty, Full O],
                     [Full O, Full X, Empty],
                     [Empty, Full O, Full X]],
         UnFinished [[Full O, Full X, Empty],
                     [Empty, Full O, Full X],
                     [Full X, Empty, Full O]]]
      , [UnFinished [[Full X, Empty, Full O],
                     [Full O, Empty, Full X],
                     [Empty, Full X, Full O]],
         UnFinished [[Empty, Full O, Full X],
                     [Full X, Empty, Full O],
                     [Full O, Full X, Empty]],
         UnFinished [[Full X, Full O, Empty],
                     [Empty, Full X, Full O],
                     [Full O, Empty, Full X]]]
      ]
