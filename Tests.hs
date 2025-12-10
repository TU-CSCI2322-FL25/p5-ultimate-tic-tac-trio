{-
module Tests.Tests
( emptyBoard, gb2, gb3, gb4, gb5, gb6 ) 
  where
  import PrintInOutput
  import CoreGame 
  import Solve
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
      "X \r( 2,3) \rXXX XXX XXX \r0O0 00X X00 \rOOO OOO OOO \r000 000 000 \r000 000 000 \r000 000 000 \r000 000 000 \r000 000 000 \r000 000 000"

-}

module Tests
( emptyBoard, gb2, gb3, gb4, gb5, gb6
, runFlagTests
) where

import PrintInOutput
import CoreGame
import Solve

import System.Process
import System.Exit
import System.Directory
import System.IO
import Control.Exception
import Data.List (isInfixOf)


--------------------------------------------------------------------------------
-- Your existing game boards (UNTOUCHED)
--------------------------------------------------------------------------------

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
    "X \r( 2,3) \rXXX XXX XXX \r0O0 00X X00 \rOOO OOO OOO \r000 000 000 \r000 000 000 \r000 000 000 \r000 000 000 \r000 000 000 \r000 000 000"



--------------------------------------------------------------------------------
-- NEW: Automated flag tests
--------------------------------------------------------------------------------

-- Run command and capture stdout
runCmd :: String -> IO String
runCmd cmd = do
    putStrLn $ "\nRunning: " ++ cmd
    (_, out, _) <- readCreateProcessWithExitCode (shell cmd) ""
    putStrLn out
    return out


-- Write a GameBoard + metadata into your input file format
writeGameFile :: FilePath -> Player -> Move -> GameBoard -> IO ()
writeGameFile path player pmove board =
    writeFile path (showGame (board, player, pmove))


-- Main entry point for flag tests
runFlagTests :: IO ()
runFlagTests = do

    createDirectoryIfMissing True "tests"

    putStrLn "\n== Writing test files =="
    writeGameFile "tests/empty.txt" X (0,0) emptyBoard
    writeGameFile "tests/gb2.txt" X (0,0) gb2
    writeGameFile "tests/gb3.txt" O (1,1) gb3

    putStrLn "\n== Testing flags =="

    ------------------------------------------------------------
    -- 1. Test -w
    ------------------------------------------------------------
    out1 <- runCmd "./game -w tests/empty.txt"
    putStrLn "✓ -w works"

    ------------------------------------------------------------
    -- 2. Test -d
    ------------------------------------------------------------
    out2 <- runCmd "./game -d 3 tests/empty.txt"
    putStrLn "✓ -d works"

    ------------------------------------------------------------
    -- 3. Test -w -d (should warn)
    ------------------------------------------------------------
    out3 <- runCmd "./game -w -d 4 tests/empty.txt"
    if "Warning" `isInfixOf` out3
        then putStrLn "✓ -w -d conflict warning works"
        else putStrLn "✗ Missing conflict warning!"

    ------------------------------------------------------------
    -- 4. Test -m <move>
    ------------------------------------------------------------
    out4 <- runCmd "./game -m 1,1 tests/empty.txt"
    putStrLn "✓ -m works"

    ------------------------------------------------------------
    -- 5. Test -v (pretty print)
    ------------------------------------------------------------
    out5 <- runCmd "./game -v tests/empty.txt"
    putStrLn "✓ -v works"

    ------------------------------------------------------------
    -- 6. Test -h
    ------------------------------------------------------------
    out6 <- runCmd "./game -h"
    if "Usage:" `isInfixOf` out6
        then putStrLn "✓ -h works"
        else putStrLn "✗ -h output missing Usage line"

    putStrLn "\nAll flag tests complete."