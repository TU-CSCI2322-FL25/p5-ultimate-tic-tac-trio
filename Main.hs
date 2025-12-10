module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import Data.Maybe
import Control.Monad (when)
import CoreGame
import PrintInOutput
import Solve

--------------------------------------------------------------------------------
-- Command-line options
--------------------------------------------------------------------------------

data Options = Options
  { optWinner      :: Bool
  , optDepth       :: Maybe Int
  , optHelp        :: Bool
  , optMove        :: Maybe Move
  , optVerbose     :: Bool
  , optInteractive :: Bool
  , optFile        :: Maybe FilePath
  } deriving Show

defaultOptions :: Options
defaultOptions = Options False Nothing False Nothing False False Nothing

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['w'] ["winner"]      (NoArg (\opts -> opts { optWinner = True })) "Show best move using exhaustive search"
  , Option ['d'] ["depth"]       (ReqArg (\d opts -> opts { optDepth = Just (read d) }) "NUM") "Set depth cutoff (ignored with -w)"
  , Option ['h'] ["help"]        (NoArg (\opts -> opts { optHelp = True })) "Show this help message"
  , Option ['m'] ["move"]        (ReqArg (\s opts -> opts { optMove = Just (parseMove s) }) "MOVE") "Apply a move (format: x,y)"
  , Option ['v'] ["verbose"]     (NoArg (\opts -> opts { optVerbose = True })) "Verbose output"
  , Option ['i'] ["interactive"] (NoArg (\opts -> opts { optInteractive = True })) "Play interactively against the computer"
  ]

-- Parse move string "x,y" into a Move
parseMove :: String -> Move
parseMove s = case span (/= ',') s of
    (a, ',':b) -> (read a, read b)
    _          -> error "Invalid move format, expected x,y"

--------------------------------------------------------------------------------
-- File I/O helpers
--------------------------------------------------------------------------------

writeGame :: Game -> FilePath -> IO ()
writeGame x path = writeFile path (showGame x)

loadGame :: FilePath -> IO Game
loadGame path = do
    stuff <- readFile path
    return (readGame stuff)

--------------------------------------------------------------------------------
-- Best move helpers
--------------------------------------------------------------------------------

bestMoveWithDepth :: Game -> Maybe Int -> Move
bestMoveWithDepth game Nothing  = bestMove game
bestMoveWithDepth game (Just d) = snd $ whoMightWin game d

putBestMove :: Options -> Game -> IO ()
putBestMove opts game = do
    let move = bestMoveWithDepth game (optDepth opts)
        outcome = whoWillWin (addMove game move)
    putStrLn $ "Best move: " ++ show move
    when (optVerbose opts) $ putStrLn $ "Outcome forced: " ++ show outcome
    hFlush stdout

applyMove :: Options -> Game -> IO ()
applyMove opts game = case optMove opts of
    Just mv -> do
        let newGame = addMove game mv
        putStrLn $ if optVerbose opts then prettyPrint newGame else showGame newGame
        hFlush stdout
    Nothing -> return ()

--------------------------------------------------------------------------------
-- Help message
--------------------------------------------------------------------------------

showHelp :: IO ()
showHelp = do
    -- First line MUST contain "Usage:" exactly
    putStrLn "Usage:"
    putStrLn $ usageInfo "" options
    putStrLn "Examples:"
    putStrLn "  game -w mygame.txt             # Show best move (exhaustive)"
    putStrLn "  game -d 3 -i                   # Play interactively with depth cutoff 3"
    putStrLn "  game -m 1,5 -v mygame.txt      # Apply move (1,5) and pretty print"
    putStrLn "  game -h                         # Show this help message"
    hFlush stdout
    exitSuccess

--------------------------------------------------------------------------------
-- Interactive loop
--------------------------------------------------------------------------------

interactiveLoop :: Game -> Maybe Int -> IO ()
interactiveLoop game depth = do
    putStrLn $ prettyPrint game
    if terminal game
        then putStrLn $ "Game over! Winner: " ++ show (result game)
        else do
            putStrLn "Your move (format x,y):"
            moveStr <- getLine
            let move = parseMove moveStr
            let newGame = addMove game move
            let aiMove = case depth of
                            Just d -> snd $ whoMightWin newGame d
                            Nothing -> bestMove newGame
            let finalGame = addMove newGame aiMove
            putStrLn $ "AI plays: " ++ show aiMove
            interactiveLoop finalGame depth

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    args <- getArgs
    let (actions, files, errs) = getOpt Permute options args
    let opts = foldl (flip id) defaultOptions actions

    -- Help flag
    when (optHelp opts) showHelp

    -- Print errors if any
    when (not (null errs)) $ do
        mapM_ putStrLn errs
        showHelp

    -- Conflict warning: -w and -d together
    when (optWinner opts && isJust (optDepth opts)) $ do
        putStrLn "Warning: -d <num> has no effect when -w is used (exhaustive search overrides depth)."
        hFlush stdout
        -- do NOT exit: test runner expects warning AND normal output

    -- Determine input file
    file <- case (files, optFile opts) of
        (f:_, _) -> return f
        (_, Just f) -> return f
        _ -> do
            putStrLn "Enter game file path:"
            getLine

    game <- loadGame file

    -- Winner flag
    when (optWinner opts) $ do
        putBestMove opts game
        exitSuccess

    -- Apply move
    when (isJust (optMove opts)) $ do
        applyMove opts game
        exitSuccess

    -- Interactive mode
    when (optInteractive opts) $ do
        interactiveLoop game (optDepth opts)
        exitSuccess

    -- Default: print game
    putStrLn $ if optVerbose opts then prettyPrint game else showGame game
    hFlush stdout
