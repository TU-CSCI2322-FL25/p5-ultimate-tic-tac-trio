module Main where

import System.Environment
import CoreGame
import printInOutput
import Solve

    writeGame :: Game -> FilePath -> IO ()
    writeGame x path = writeFile path (showGame x)

    loadGame :: FilePath -> IO Game
    loadGame path = do
        stuff <- readFile path
        return (readGame stuff)

    putBestMove :: Game -> IO ()
    putBestMove x = do 
        let move = bestMove x
            outcome = whoWillWin(addMove x move)
        putStrLn ("Best move: " ++ show move)
        putStrLn ("Outcome forced: " ++ show outcome)
    
    main :: IO ()
    main = do
        args <- getArgs
        path <- case args of
            (b:_) -> return b
            []  -> do
                putStrLn "Enter game file path:"
                getLine

        game <- loadGame path
        putBestMove game
--story 15
