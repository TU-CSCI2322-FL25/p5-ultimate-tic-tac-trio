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
