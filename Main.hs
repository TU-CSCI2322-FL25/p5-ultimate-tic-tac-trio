{-
module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Maybe
import CoreGame
import PrintInOutput
import Solve
import Control.Monad (when, unless)

data Options = Options
    { optHelp       :: Bool
    , optVerbose    :: Bool
    , optWinner     :: Bool
    , optDepth      :: Maybe Int
    , optMove       :: Maybe Move
    , optInteractive :: Bool
    , fileName      :: Maybe String
    }

defaultOptions :: Options
defaultOptions = Options
    { optHelp       = False
    , optVerbose    = False
    , optWinner     = False
    , optDepth      = Nothing
    , optMove       = Nothing
    , optInteractive = False
    , fileName      = Nothing
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]        (NoArg  (\opts -> opts { optHelp = True })) "Show help message"
    , Option ['v'] ["verbose"]     (NoArg  (\opts -> opts { optVerbose = True })) "Verbose output"
    , Option ['w'] ["winner"]      (NoArg  (\opts -> opts { optWinner = True })) "Compute full exhaustive winner"
    , Option ['d'] ["depth"]       (ReqArg (\n opts -> opts { optDepth = readMaybe n }) "N") "Set search depth"
    , Option ['m'] ["move"]        (ReqArg (\mv opts -> opts { optMove = parseMove mv }) "X,Y") "Apply a move (1-indexed)"
    , Option ['i'] ["interactive"] (NoArg  (\opts -> opts { optInteractive = True })) "Play interactively against the computer"
    ]

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x,"")] -> Just x
    _        -> Nothing

parseMove :: String -> Maybe Move
parseMove s = case break (== ',') s of
    (a, ',':b) -> case (reads a, reads b) of
        ([(x,"")], [(y,"")]) -> Just (x-1, y-1)
        _                     -> Nothing
    _ -> Nothing

-- Help message
helpMessage :: String
helpMessage = usageInfo "Usage: Game [OPTIONS] <gamefile>" options

applyMoveAndPrint :: Game -> Move -> Bool -> IO ()
applyMoveAndPrint g mv verbose =
    if mv `elem` legalMoves g
    then let g' = addMove g mv
         in if verbose then putStrLn (prettyPrint g') else putStrLn (showGame g')
    else putStrLn "Illegal move."

playInteractive :: Game -> Int -> Bool -> IO ()
playInteractive game depth verbose = do
    putStrLn $ "\nCurrent board:\n" ++ showGame game
    if terminal game
    then putStrLn $ "Game over! Outcome: " ++ show (rateGame game)
    else do
        case currentPlayer game of
            X -> do  --x played by person
                putStrLn "Your move (format X,Y, 1-indexed):"
                line <- getLine
                case parseMove line of
                    Just mv -> 
                        if mv `elem` legalMoves game
                        then playInteractive (addMove game mv) depth verbose
                        else do
                            putStrLn "Illegal move. Try again."
                            playInteractive game depth verbose
                    Nothing -> do
                        putStrLn "Invalid input. Try again."
                        playInteractive game depth verbose
            O -> do  --o played by computer
                let d = fromMaybe 5 (Just depth)
                    (_, mv) = whoMightWin game d
                putStrLn $ "Computer plays: " ++ show mv
                let newGame = addMove game mv
                when verbose $ putStrLn $ "Board after move:\n" ++ prettyPrint newGame
                playInteractive newGame depth verbose

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOpts, errs) = getOpt Permute options args
    when (not (null errs)) $ do
        putStrLn $ concat errs
        putStrLn helpMessage
        exitFailure

    let opts = foldl (flip id) defaultOptions actions
        opts' = opts { fileName = case nonOpts of
                                        (f:_) -> Just f
                                        []    -> Nothing }

    --help
    when (optHelp opts') $ do
        putStrLn helpMessage
        exitSuccess

    --get game file
    path <- case fileName opts' of
        Just f  -> return f
        Nothing -> if optInteractive opts' then return "" else do
            putStrLn "Enter game file path:"
            getLine

    game <- if path /= "" then loadGame path else return initialGame

    --interactive
    if optInteractive opts'
    then do
        let depth = fromMaybe 5 (optDepth opts')
        playInteractive game depth (optVerbose opts')
    else do
        --move
        case optMove opts' of
            Just mv -> applyMoveAndPrint game mv (optVerbose opts')
            Nothing -> do
                --depth aand winner case (goes to warning)
                when (optWinner opts' && isJust (optDepth opts')) $
                    putStrLn $ "Warning: --depth ignored because --winner is used."

                if optWinner opts'
                then do
                    let mv     = bestMove game
                        forced = whoWillWin (addMove game mv)
                        final  = whoWillWin game
                    putStrLn $ "Best move: " ++ show mv
                    putStrLn $ "Outcome forced: " ++ show forced
                    putStrLn $ "Winner (exhaustive): " ++ show final
                else do
                    let depth = fromMaybe 5 (optDepth opts')
                        mv    = bestMoveDepth depth game
                        outcome = whoWillWinDepth depth (addMove game mv)
                    if optVerbose opts'
                    then do
                        putStrLn $ "Best move: " ++ show mv
                        putStrLn $ "Outcome: " ++ show outcome
                    else do
                        putStrLn $ "Best move: " ++ show mv
                        putStrLn $ "Outcome forced (depth " ++ show depth ++ "): " ++ show outcome
-}

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

-- Command-line options
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
  [ Option ['w'] ["winner"]   (NoArg (\opts -> opts { optWinner = True })) "Show best move using exhaustive search"
  , Option ['d'] ["depth"]    (ReqArg (\d opts -> opts { optDepth = Just (read d) }) "NUM") "Set depth cutoff (ignored with -w)"
  , Option ['h'] ["help"]     (NoArg (\opts -> opts { optHelp = True })) "Show this help message"
  , Option ['m'] ["move"]     (ReqArg (\s opts -> opts { optMove = Just (parseMove s) }) "MOVE") "Apply a move (format: x,y)"
  , Option ['v'] ["verbose"]  (NoArg (\opts -> opts { optVerbose = True })) "Verbose output"
  , Option ['i'] ["interactive"] (NoArg (\opts -> opts { optInteractive = True })) "Play interactively against the computer"
  ]

-- Parse move string "x,y" into a Move
parseMove :: String -> Move
parseMove s = case span (/= ',') s of
    (a, ',':b) -> (read a, read b)
    _          -> error "Invalid move format, expected x,y"

-- Write and load helpers
writeGame :: Game -> FilePath -> IO ()
writeGame x path = writeFile path (showGame x)

loadGame :: FilePath -> IO Game
loadGame path = do
    stuff <- readFile path
    return (readGame stuff)

-- Show best move for Story 22
putBestMove :: Options -> Game -> IO ()
putBestMove opts game = do
    let move = bestMove game
        outcome = whoWillWin (addMove game move)
    putStrLn $ "Best move: " ++ show move
    when (optVerbose opts) $ putStrLn $ "Outcome forced: " ++ show outcome

-- Apply a move and print resulting game
applyMove :: Options -> Game -> IO ()
applyMove opts game = case optMove opts of
    Just mv -> do
        let newGame = addMove game mv
        putStrLn $ if optVerbose opts then prettyPrint newGame else showGame newGame
    Nothing -> return ()

-- Help message
showHelp :: IO ()
showHelp = do
    putStrLn $ usageInfo header options
    putStrLn "Examples:"
    putStrLn "  game -w mygame.txt             # Show best move (exhaustive)"
    putStrLn "  game -d 3 -i                   # Play interactively with depth cutoff 3"
    putStrLn "  game -m 1,5 -v mygame.txt      # Apply move (1,5) and pretty print"
    putStrLn "  game -h                         # Show this help message"
    exitSuccess
  where
    header = "Usage: game [OPTIONS] [FILE]"

-- Main
main :: IO ()
main = do
    args <- getArgs
    let (actions, files, errs) = getOpt Permute options args
    let opts = foldl (flip id) defaultOptions actions

    when (optHelp opts) showHelp
    when (not (null errs)) $ do
        mapM_ putStrLn errs
        showHelp

    -- Conflict warning: -w and -d together
    when (optWinner opts && isJust (optDepth opts)) $
        putStrLn "Warning: -d <num> has no effect when -w is used (exhaustive search overrides depth)."

    -- Determine input file
    file <- case (files, optFile opts) of
        (f:_, _) -> return f
        (_, Just f) -> return f
        _ -> do
            putStrLn "Enter game file path:"
            getLine

    game <- loadGame file

    -- Story 22: Winner flag (exhaustive search)
    when (optWinner opts) $ do
        putBestMove opts game
        exitSuccess

    -- Story 25: Apply a move
    when (isJust (optMove opts)) $ do
        applyMove opts game
        exitSuccess

    -- Story 27: Interactive mode (extra credit)
    when (optInteractive opts) $ do
        interactiveLoop game (optDepth opts)
        exitSuccess

    -- Default: print game
    putStrLn $ if optVerbose opts then prettyPrint game else showGame game

-- Interactive game loop
interactiveLoop :: Game -> Maybe Int -> IO ()
interactiveLoop game depth = do
    putStrLn $ prettyPrint game
    if isJust (checkWinner game)
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