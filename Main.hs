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

initialGame :: Game
initialGame = gameStart

-- | Load game from file
loadGame :: FilePath -> IO Game
loadGame path = do
    contents <- readFile path
    return $ readGame contents

-- | Depth-limited best move
bestMoveDepth :: Int -> Game -> Move
bestMoveDepth depth g = snd $ whoMightWin g depth

-- | Depth-limited forced outcome
whoWillWinDepth :: Int -> Game -> Winner
whoWillWinDepth depth g =
    case currentPlayer g of
        X -> case fst (whoMightWin g depth) of
                10  -> Win X
                0   -> Draw
                _   -> Win O
        O -> case fst (whoMightWin g depth) of
                -10 -> Win O
                0   -> Draw
                _   -> Win X

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