module EvenGames
  ( even1, even2, even3,
  ) where

import PrintInOutput
import CoreGame
import Solve

even1 :: Game
even1 =
  ( board, X, (1,1) )
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))
    board =
      [ [UnFinished [[Empty, Full X, Empty],
                     [Full O, Empty, Empty],
                     [Empty, Empty, Full X]],
         UnFinished [[Empty, Empty, Empty],
                     [Empty, Full O, Empty],
                     [Full X, Empty, Empty]],
         emptySB]
      , [emptySB,
         UnFinished [[Full X, Empty, Full O],
                     [Empty, Full O, Empty],
                     [Empty, Empty, Full X]],
         emptySB]
      , [emptySB,
         emptySB,
         UnFinished [[Empty, Empty, Empty],
                     [Empty, Full X, Empty],
                     [Full O, Empty, Empty]]]
      ]

even2 :: Game
even2 =
  ( board, O, (2,2))
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))
    board =
      [ [UnFinished [[Full X, Empty, Empty],
                     [Empty, Full O, Empty],
                     [Empty, Empty, Full X]],
         emptySB,
         UnFinished [[Empty, Full O, Empty],
                     [Empty, Full X, Empty],
                     [Full O, Empty, Empty]]]
      , [emptySB,
         UnFinished [[Empty, Full X, Empty],
                     [Full O, Empty, Full X],
                     [Empty, Empty, Empty]],
         emptySB]
      , [emptySB,
         UnFinished [[Full X, Empty, Full O],
                     [Empty, Empty, Full X],
                     [Empty, Full O, Empty]],
         emptySB]
      ]

even3 :: Game
even3 =
  ( board, X, (3,3))
  where
    emptySB = UnFinished (replicate 3 (replicate 3 Empty))
    board =
      [ [emptySB,
         UnFinished [[Empty, Full O, Empty],
                     [Full X, Empty, Empty],
                     [Empty, Full O, Empty]],
         emptySB]
      , [emptySB,
         UnFinished [[Full X, Empty, Full O],
                     [Empty, Full X, Empty],
                     [Empty, Empty, Full O]],
         emptySB]
      , [UnFinished [[Empty, Empty, Empty],
                     [Full O, Empty, Full X],
                     [Empty, Full X, Empty]],
         emptySB,
         emptySB]
      ]
