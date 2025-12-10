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
import Data.Maybe
import CoreGame
import PrintInOutput
import Solve
import Control.Monad (when, unless)

-- | Command line options
data Options = Options
    { optHelp        :: Bool
    , optVerbose     :: Bool
    , optWinner      :: Bool
    , optDepth       :: Maybe Int
    , optMove        :: Maybe Move
    , optInteractive :: Bool
    , fileName       :: Maybe String
    }

defaultOptions :: Options
defaultOptions = Options
    { optHelp        = False
    , optVerbose     = False
    , optWinner      = False
    , optDepth       = Nothing
    , optMove        = Nothing
    , optInteractive = False
    , fileName       = Nothing
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

-- | Safe read
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x,"")] -> Just x
    _        -> Nothing

-- | Parse user input move "X,Y" into zero-based Move
parseMove :: String -> Maybe Move
parseMove s = case break (== ',') s of
    (a, ',':b) -> case (reads a, reads b) of
        ([(x,"")], [(y,"")]) -> Just (x-1, y-1)
        _                     -> Nothing
    _ -> Nothing

-- | Help message
helpMessage :: String
helpMessage = usageInfo "Usage: Game [OPTIONS] <gamefile>" options

-- | Initial game (empty board)
initialGame :: Game
initialGame = GameStart  -- replace with whatever CoreGame defines as starting game

-- | Check if game is over
isTerminalGame :: Game -> Bool
isTerminalGame g = isJust (whoWillWin g)

-- | Determine current player
currentPlayer :: Game -> Player
currentPlayer g = if even (length (legalMoves g)) then X else O
-- adjust if your Game type has a player field

-- | Apply move and print board
applyMoveAndPrint :: Game -> Move -> Bool -> IO ()
applyMoveAndPrint g mv verbose =
    if mv `elem` legalMoves g
    then let g' = addMove g mv
         in if verbose then putStrLn (prettyPrint g') else putStrLn (showGame g')
    else putStrLn "Illegal move."

-- | Interactive play
playInteractive :: Game -> Int -> Bool -> IO ()
playInteractive game depth verbose = do
    putStrLn $ "\nCurrent board:\n" ++ showGame game
    if isTerminalGame game
    then case whoWillWin game of
            Just outcome -> putStrLn $ "Game over! Outcome: " ++ show outcome
            Nothing      -> putStrLn "Game over! Outcome unknown."
    else do
        case currentPlayer game of
            X -> do
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
            O -> do
                let mv = bestMove game
                putStrLn $ "Computer plays: " ++ show mv
                let newGame = addMove game mv
                when verbose $ putStrLn $ "Board after move:\n" ++ prettyPrint newGame
                playInteractive newGame depth verbose

-- | Main
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

    -- Help
    when (optHelp opts') $ do
        putStrLn helpMessage
        exitSuccess

    -- Load game
    path <- case fileName opts' of
        Just f  -> return f
        Nothing -> if optInteractive opts' then return "" else do
            putStrLn "Enter game file path:"
            getLine

    game <- if path /= "" 
            then readGame <$> readFile path
            else return initialGame

    -- Interactive mode
    if optInteractive opts'
    then do
        let depth = fromMaybe 5 (optDepth opts')
        playInteractive game depth (optVerbose opts')
    else do
        -- Single move
        case optMove opts' of
            Just mv -> applyMoveAndPrint game mv (optVerbose opts')
            Nothing -> do
                when (optWinner opts' && isJust (optDepth opts')) $
                    putStrLn $ "Warning: --depth ignored because --winner is used."

                if optWinner opts'
                then do
                    let mv     = bestMove game
                        final  = whoWillWin (addMove game mv)
                    putStrLn $ "Best move: " ++ show mv
                    putStrLn $ "Winner (exhaustive): " ++ maybe "Unknown" show final
                else do
                    let depth = fromMaybe 5 (optDepth opts')
                        mv    = bestMove game  -- use only bestMove
                        outcome = whoWillWin (addMove game mv)
                    if optVerbose opts'
                    then do
                        putStrLn $ "Best move: " ++ show mv
                        putStrLn $ "Outcome: " ++ maybe "Unknown" show outcome
                    else do
                        putStrLn $ "Best move: " ++ show mv
                        putStrLn $ "Outcome (depth " ++ show depth ++ "): " ++ maybe "Unknown" show outcome
