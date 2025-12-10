module Solve
( whoWillWin
, bestMove
,terminal
,whoMightWin
,result
) where

import CoreGame

---story 9


whoWillWin :: Game -> Winner
whoWillWin g@(board, player, premove)
    | terminal g = result g
    | otherwise =
        let moves = legalMoves g
            outcomes = [whoWillWin (addMove g m) | m <- moves]
        in bestOutcome player outcomes


bestOutcome :: Player -> [Winner] -> Winner
bestOutcome p outcomes
    | any (== Win p) outcomes = Win p
    | any (== Draw) outcomes  = Draw
    | otherwise               = Win (nextPlaya p)

result :: Game -> Winner
result g = case checkWinner g of
    Just w  -> w
    Nothing -> error "Game is not finished"


terminal :: Game -> Bool
terminal g = case checkWinner g of
    Just _  -> True
    Nothing -> False
----- story 9 done

-- story 10
bestMove :: Game -> Move
bestMove g@(board, player, premove)
    |null moves = error "No legal moves are available"
    |not(null winningMoves) = head winningMoves
    |not (null drawMoves) = head drawMoves
    |otherwise = head moves
        where
        moves = legalMoves g
        outcome x = whoWillWin (addMove g x)
        winningMoves = [x | x <- moves, outcome x == Win player]
        drawMoves = [x | x <- moves, outcome x == Draw]
-- end story 10

type Rating = Int

rateGame :: Game -> Rating
rateGame (board,_,_) =
    case checkWinner (board, X, (0,0)) of
        Just (Win X) -> 10  --x wins and gets highest score of 10
        Just (Win O) -> -10 --o
        Just Draw -> 0
        Nothing -> evaluateBoard board

evaluateBoard :: GameBoard -> Int
evaluateBoard board =
    scoreSmallBoards flatBoards + sum (map scoreInSmallboard flatBoards)
    where
        flatBoards = concat board --3*3 into list of 9

--if same player wins adjacent boards +1 point
scoreSmallBoards :: [SmallBoard] -> Int
scoreSmallBoards (a:b:rest) =
    scorePair a b + scoreSmallBoards (b:rest)
  where
    scorePair (Finished (Win X)) (Finished (Win X)) =  1
    scorePair (Finished (Win O)) (Finished (Win O)) = -1
    scorePair _ _ = 0
scoreSmallBoards _ = 0

--boxes in small board
scoreInSmallboard :: SmallBoard -> Int
scoreInSmallboard (Finished _) = 0   -- finished boards dont contribute

scoreInSmallboard (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) =
    sum $ map scoreLine wins
  where
    -- All possible lines in the smallboard: rows, columns, diagonals
    wins = [[a,b,c],[d,e,f],[g,h,i],   -- rows
            [a,d,g],[b,e,h],[c,f,i],   -- columns
            [a,e,i],[c,e,g]]           -- diagonals

    -- Score a line: +1 if X has 2 and one empty, -1 if O has 2 and one empty
    scoreLine :: [Square] -> Int
    scoreLine line
      | length (filter (== Full X) line) == 2 && length (filter (== Empty) line) == 1 = 1
      | length (filter (== Full O) line) == 2 && length (filter (== Empty) line) == 1 = -1
      | otherwise = 0

--win 10
--loss -10

--1 point for 2 smallboard wins in a row
-- .5 point for 2 boxes in a row per smallboard that has not been won
--end story 17


-- 18 & 19
whoMightWin :: Game -> Int -> (Rating, Move)
whoMightWin g depth =
    let moves = legalMoves g
    in if null moves
        then (rateGame g, (0,0))
        else chooseBest g depth moves

chooseBest :: Game -> Int -> [Move] -> (Rating, Move)
chooseBest g@(board, player, premove) depth moves =
    case player of
        X -> maximize g depth moves  -- X tries to reach rating = 10
        O -> minimize g depth moves  -- O tries to reach rating = -10

maximize :: Game -> Int -> [Move] -> (Rating, Move)
maximize _ _ [] = error "maximize: no moves"

maximize g depth (m:ms) =
    let r = minimax (addMove g m) (depth-1)
    in if r == 10
           then (10, m)              -- PERFECT for X → stop immediately
           else compareMore r m ms   -- otherwise check more
  where
    compareMore bestR bestM [] = (bestR, bestM)
    compareMore bestR bestM (move:rest) =
        let r = minimax (addMove g move) (depth-1)
        in if r == 10
              then (10, move)        -- stop early!
              else if r > bestR
                      then compareMore r move rest
                      else compareMore bestR bestM rest

minimize :: Game -> Int -> [Move] -> (Rating, Move)
minimize _ _ [] = error "minimize: no moves"

minimize g depth (m:ms) =
    let r = minimax (addMove g m) (depth-1)
    in if r == -10
           then (-10, m)             -- PERFECT for O → stop immediately
           else compareMore r m ms
  where
    compareMore bestR bestM [] = (bestR, bestM)
    compareMore bestR bestM (move:rest) =
        let r = minimax (addMove g move) (depth-1)
        in if r == -10
              then (-10, move)
              else if r < bestR
                      then compareMore r move rest
                      else compareMore bestR bestM rest

minimax :: Game -> Int -> Rating
minimax g depth
    | terminal g = rateGame g
    | depth == 0 = rateGame g
    | otherwise  =
        let moves = legalMoves g in
        case moves of
            [] -> rateGame g
            _  ->
                case currentPlayer g of
                    X -> maxRating g depth moves
                    O -> minRating g depth moves

maxRating :: Game -> Int -> [Move] -> Rating
maxRating _ _ [] = error "maxRating: empty"
maxRating g depth (m:ms) =
    let r = minimax (addMove g m) (depth-1)
    in if r == 10 then 10 else scan r ms
  where
    scan best [] = best
    scan best (move:rest) =
        let r = minimax (addMove g move) (depth-1)
        in if r == 10
              then 10
              else scan (max best r) rest


minRating :: Game -> Int -> [Move] -> Rating
minRating _ _ [] = error "minRating: empty"
minRating g depth (m:ms) =
    let r = minimax (addMove g m) (depth-1)
    in if r == -10 then -10 else scan r ms
  where
    scan best [] = best
    scan best (move:rest) =
        let r = minimax (addMove g move) (depth-1)
        in if r == -10
              then -10
              else scan (min best r) rest

currentPlayer :: Game -> Player
currentPlayer (_, p, _) = p
