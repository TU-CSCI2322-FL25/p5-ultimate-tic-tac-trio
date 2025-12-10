module EarlyGames
  ( early1, early2, early3, early4,
  ) where

import PrintInOutput
import CoreGame
import Solve

early1 :: Game
early1 =
  ( board, X, (1,1))
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))

    sb1 = UnFinished [[Full O, Empty, Empty],
                       [Empty,  Empty, Empty],
                       [Empty,  Empty, Empty]]

    sb5 = UnFinished [[Empty, Empty, Empty],
                       [Empty, Full X, Empty],
                       [Empty, Empty, Empty]]

    board =
      [ [sb1, emptySB, emptySB]
      , [emptySB, sb5,  emptySB]
      , [emptySB, emptySB, emptySB]
      ]

early2 :: Game
early2 =
  ( board, O, (2,3) )
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))

    sb5 = UnFinished [[Empty, Empty, Empty],
                       [Full X, Empty, Empty],
                       [Empty, Empty, Empty]]

    sb2 = UnFinished [[Empty, Full O, Empty],
                       [Empty, Empty, Empty],
                       [Empty, Empty, Empty]]

    sb4 = UnFinished [[Empty, Empty, Full X],
                       [Empty, Empty, Empty],
                       [Empty, Empty, Empty]]

    board =
      [ [emptySB, sb2,  emptySB]
      , [sb4,   sb5,   emptySB]
      , [emptySB, emptySB, emptySB]
      ]

early3 :: Game
early3 =
  ( board, X, (3,2) )
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))

    sb1 = UnFinished [[Full X, Empty, Empty],
                      [Empty,  Empty, Empty],
                      [Empty, Full O, Empty]]

    sb8 = UnFinished [[Empty, Empty, Empty],
                       [Full O, Empty, Empty],
                       [Empty, Empty, Empty]]

    sb6 = UnFinished [[Empty, Empty, Full X],
                       [Empty, Empty, Empty],
                       [Empty, Empty, Empty]]

    board =
      [ [sb1, emptySB, emptySB]
      , [emptySB, emptySB, sb6]
      , [emptySB, sb8, emptySB]
      ]

early4 :: Game
early4 =
  ( board, O, (1,3))
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))

    sb5 = UnFinished [[Empty, Full X, Empty],
                       [Empty, Empty, Empty],
                       [Empty, Empty, Empty]]

    sb2 = UnFinished [[Empty, Empty, Empty],
                       [Empty, Empty, Full O],
                       [Empty, Empty, Empty]]

    sb6 = UnFinished [[Empty, Empty, Empty],
                       [Full X, Empty, Empty],
                       [Empty, Empty, Empty]]

    sb7 = UnFinished [[Empty, Empty, Empty],
                       [Empty, Empty, Empty],
                       [Full O, Empty, Empty]]

    sb3 = UnFinished [[Empty, Empty, Empty],
                       [Empty, Empty, Empty],
                       [Empty, Full X, Empty]]

    board =
      [ [emptySB, sb2, sb3]
      , [emptySB, sb5, sb6]
      , [sb7, emptySB, emptySB]
      ]
