module DominatingGames
  ( dom1, dom2, dom3,
    dom1gb
  ) where

import PrintInOutput
import CoreGame
import Solve

dom1 :: Game
dom1 = (dom1gb, O, (1,3))

dom1gb :: GameBoard
dom1gb =
  [ [ sb1, sb2, sb3 ]
  , [ sb4, sb5, sb6 ]
  , [ sb7, sb8, sb9 ]
  ]
  where
    sb1 = UnFinished [[Empty,Full X,Empty],
                       [Full X,Empty,Full X],
                       [Full O,Full O,Full O]]

    sb2 = Finished (Win O)

    sb3 = UnFinished [[Empty,Full X,Empty],
                       [Empty,Full X,Empty],
                       [Empty,Full X,Empty]]

    sb4 = Finished (Win X)

    sb5 = UnFinished [[Empty,Full X,Empty],
                       [Empty,Full O,Empty],
                       [Empty,Full X,Empty]]

    sb6 = Finished (Win X)

    sb7 = Finished (Win O)

    sb8 = Finished Draw

    sb9 = UnFinished [[Empty,Full X,Empty],
                       [Empty,Full X,Empty],
                       [Empty,Empty,Empty]]


dom2 :: Game
dom2 =
  ( board, X, (1,3))
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))

    winX = Finished (Win X)

    sb3 = UnFinished [[Full O, Empty,   Full X],
                       [Empty,  Empty,   Empty],
                       [Empty,  Full X,  Empty]]

    sb4 = UnFinished [[Empty, Full X,  Empty],
                       [Full O, Empty,  Empty],
                       [Empty, Empty,   Empty]]

    sb6 = UnFinished [[Empty, Empty, Full X],
                       [Empty, Full O, Empty],
                       [Empty, Empty,  Empty]]

    sb8 = UnFinished [[Empty, Full X, Empty],
                       [Empty, Empty,  Full O],
                       [Empty, Empty,  Empty]]

    board =
      [ [winX, emptySB, sb3]
      , [sb4, winX, sb6]
      , [emptySB, sb8, emptySB]
      ]

dom3 :: Game
dom3 =
  ( board, O, (3,1) )
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))

    winO = Finished (Win O)

    sb3 = UnFinished [[Empty,   Full O, Empty],
                       [Empty,   Empty,  Empty],
                       [Full X,  Empty,  Empty]]

    sb4 = UnFinished [[Full O, Empty,   Empty],
                       [Empty,  Empty,   Empty],
                       [Empty,  Full X,  Empty]]

    sb6 = UnFinished [[Empty, Full O, Empty],
                       [Empty, Empty,  Empty],
                       [Empty, Empty,  Full X]]

    sb7 = UnFinished [[Empty, Full O,  Full X],
                       [Empty, Empty,   Empty],
                       [Empty, Empty,   Empty]]

    sb8 = UnFinished [[Empty,  Empty,  Empty],
                       [Full O, Empty,  Empty],
                       [Empty,  Empty,  Full X]]

    board =
      [ [emptySB, winO, sb3]
      , [sb4, emptySB, sb6]
      , [sb7, sb8, winO]
      ]
