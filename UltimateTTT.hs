data Player = X | O deriving (Show, Eq)
-- helper function to change player 

data Winner = Win Player | Draw | OnGoing deriving (Eq, Show)

data Square = Full Player | Empty deriving (Eq, Show)

type GameBoard = [[SmallBoard]]

type Game = (GameBoard, Player) 

data SmallBoard = UnFinished [[Square]] | Finished Winner deriving (Eq, Show)

type Loc = (Int, Int)

type prevMove = Loc


--helper function to change prev move and keep track like the player helper function


legalMoves :: Game -> Loc -> [Loc]
legalMoves (board, _) (bb, sq) =
    case getSmallBoardFlat board sq of
        Nothing -> []
        Just sb ->
            if isPlayable sb
                then -- can only play inside sq smallboard
                    [ (sq, s) | s <- emptySquaresFlat sb ]
                else -- free choice
                    [ (b, s) | (b, sb') <- zip [0..] board, isPlayable sb', s <- emptySquaresFlat sb']

flattenBoard :: GameBoard -> [(Int, SmallBoard)]
flattenBoard gb =
    [ (boardIdx, sb) | (boardRow, row) <- zip [0..] gb, (boardCol, sb)  <- zip [0..] row, let boardIdx = boardRow * 3 + boardCol]

getSmallBoardFlat :: GameBoard -> Int -> Maybe SmallBoard
getSmallBoardFlat gb idx =
    listToMaybe
        [ sb | (i, sb) <- flattenBoard gb, i == idx]

emptySquaresFlat :: SmallBoard -> [Int]
emptySquaresFlat (Finished _) = []
emptySquaresFlat (UnFinished rows) =
    [ r * 3 + c | (r, row) <- zip [0..] rows, (c, sq)  <- zip [0..] row, sq == Empty]

isPlayable :: SmallBoard -> Bool
isPlayable (UnFinished _) = True
isPlayable _              = False