module Tests.Tests
( emptyBoard, gb2, gb3, gb4, gb5, gb6 ) where

import CoreGame
  emptyBoard :: GameBoard
  emptyBoard =
    [ [emptySmall, emptySmall, emptySmall]
    , [emptySmall, emptySmall, emptySmall]
    , [emptySmall, emptySmall, emptySmall]
    ]
      where
          emptySmall = UnFinished (replicate 3 (replicate 3 Empty))

  gb2 :: GameBoard
  gb2 =
    [ [empty, winX, empty]
    , [draw, empty, winO]
    , [empty, empty, draw]
    ]
      where
          empty = UnFinished (replicate 3 (replicate 3 Empty))
          winX = Finished (Win X)
          winO = Finished (Win O)
          draw = Finished Draw
  
  gb3 :: GameBoard
  gb3 =
    [ [sb1, sb2, sb3]
    , [sb4, sb5, sb6]
    , [sb7, sb8, sb9]
    ]
    where
      sb1 = UnFinished [[Empty,Full X,Empty],[Full O,Empty,Empty],[Empty,Empty,Full O]]
      sb2 = UnFinished [[Full X,Full O,Empty],[Empty,Empty,Full O],[Empty,Full X,Empty]]
      sb3 = Finished (Win O)
      sb4 = UnFinished [[Full X,Empty,Empty],[Full O,Full X,Empty],[Empty,Empty,Empty]]
      sb5 = UnFinished [[Empty,Empty,Empty],[Empty,Full O,Full X],[Empty,Empty,Empty]]
      sb6 = Finished (Win X)
      sb7 = UnFinished [[Empty,Empty,Empty],[Empty,Full X,Empty],[Full O,Empty,Empty]]
      sb8 = Finished Draw
      sb9 = UnFinished [[Empty,Empty,Full X],[Empty,Empty,Empty],[Full O,Empty,Empty]]
  
  gb4 :: GameBoard
  gb4 =
    [ [fullX, fullO, fullX]
    , [fullO, fullX, fullO]
    , [fullO, empty, fullX]
    ]
    where
      fullX = UnFinished (replicate 3 (replicate 3 (Full X)))
      fullO = UnFinished (replicate 3 (replicate 3 (Full O)))
      empty = UnFinished (replicate 3 (replicate 3 Empty))
  
  gb5 :: GameBoard
  gb5 =
    [ [sb1,sb2,sb3]
    , [sb4,sb5,sb6]
    , [sb7,sb8,sb9]
    ]
    where
      sb1 = UnFinished [[Empty,Full X,Full O],[Empty,Empty,Empty],[Full O,Empty,Full X]]
      sb2 = Finished (Win X)
      sb3 = UnFinished [[Empty,Empty,Empty],[Empty,Full O,Full O],[Full X,Empty,Empty]]
      sb4 = UnFinished [[Full O,Empty,Empty],[Empty,Full X,Empty],[Empty,Empty,Empty]]
      sb5 = Finished Draw
      sb6 = UnFinished [[Empty,Full X,Empty],[Empty,Empty,Empty],[Full X,Empty,Full O]]
      sb7 = UnFinished [[Full X,Full O,Empty],[Empty,Empty,Empty],[Empty,Full O,Full X]]
      sb8 = UnFinished [[Empty,Empty,Full O],[Full X,Empty,Empty],[Empty,Empty,Empty]]
      sb9 = Finished (Win O)
  
  gb6 = 
      "X
      (2,3)
      XXX XXX XXX
      0O0 00X X00
      OOO OOO OOO
      000 000 000
      000 000 000
      000 000 000
      000 000 000
      000 000 000
      000 000 000"
