data Player = X | O deriving (Show, Eq)
-- helper function to change player 

data Winner = Win Player | Draw deriving (Eq, Show)

data Square = Full Player | Empty deriving (Eq, Show)

type GameBoard = [[SmallBoard]]

type Game = (GameBoard, Player, Move) 

data SmallBoard = UnFinished [[Square]] | Finished Winner deriving (Eq, Show)

type Move = (Int, Int)


--helper function to change prev move and keep track like the player helper function

addMove :: Game -> Move -> Game
addMove (board, playa, premove) move = if move `elem` legalm then (updateBoard board move playa, nextPlaya playa, move) else error "That is not a legal move punk >:("
	where
	
	legalm = legalMoves (board, playa, premove)

	updateBoard :: GameBoard -> Move -> Player -> GameBoard
	updateBoard [[a,b,c],[d,e,f],[g,h,i]] (1, x) p = [[checkFinished(updateSquare a x p),b,c],[d,e,f],[g,h,i]]
        updateBoard [[a,b,c],[d,e,f],[g,h,i]] (2, x) p = [[a,checkFinished(updateSquare b x p),c],[d,e,f],[g,h,i]]
        updateBoard [[a,b,c],[d,e,f],[g,h,i]] (3, x) p = [[a,b,checkFinished(updateSquare c x p)],[d,e,f],[g,h,i]]
        updateBoard [[a,b,c],[d,e,f],[g,h,i]] (4, x) p = [[a,b,c],[checkFinished(updateSquare d x p),e,f],[g,h,i]]
        updateBoard [[a,b,c],[d,e,f],[g,h,i]] (5, x) p = [[a,b,c],[d,checkFinished(updateSquare e x p),f],[g,h,i]]
        updateBoard [[a,b,c],[d,e,f],[g,h,i]] (6, x) p = [[a,b,c],[d,e,checkFinished(updateSquare f x p)],[g,h,i]]
        updateBoard [[a,b,c],[d,e,f],[g,h,i]] (7, x) p = [[a,b,c],[d,e,f],[checkFinished(updateSquare g x p),h,i]]
        updateBoard [[a,b,c],[d,e,f],[g,h,i]] (8, x) p = [[a,b,d],[d,e,f],[g,checkFinished(updateSquare h x p),i]]
        updateBoard [[a,b,c],[d,e,f],[g,h,i]] (9, x) p = [[a,b,c],[d,e,f],[g,h,checkFinished(updateSquare i x p)]]


	updateSquare :: SmallBoard -> Int -> Player -> SmallBoard
	updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 1 p = UnFinished [[Full p,b,c],[d,e,f],[g,h,i]] 
        updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 2 p = UnFinished [[a,Full p,c],[d,e,f],[g,h,i]]
        updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 3 p = UnFinished [[a,b,Full p],[d,e,f],[g,h,i]]
        updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 4 p = UnFinished [[a,b,c],[Full p,e,f],[g,h,i]]
        updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 5 p = UnFinished [[a,b,c],[d,Full p,f],[g,h,i]]
        updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 6 p = UnFinished [[a,b,c],[d,e,Full p],[g,h,i]]
        updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 7 p = UnFinished [[a,b,c],[d,e,f],[Full p,h,i]]
        updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 8 p = UnFinished [[a,b,c],[d,e,f],[g,Full p,i]]
        updateSquare (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) 9 p = UnFinished [[a,b,c],[d,e,f],[g,h,Full p]]
	updateSquare _ x _ = error "Your move must be a number index must be 1 - 9"


	checkFinished :: SmallBoard -> SmallBoard
	checkFinished (Finished sb) = Finished sb
	checkFinished (UnFinished [[a,b,c],[d,e,f],[g,h,i]]) = 
		let wins = [[a,b,c],[d,e,f],[g,h,i],[a,d,g],[b,e,h],[c,f,i],[a,e,i],[c,e,g]]
		    squares = [a,b,c,d,e,f,g,h,i]
		in if any(all(==Full X)) wins then Finished (Win X) else if any (all (==Full O)) wins then Finished (Win O) else if any (==Empty) squares then UnFinished [[a,b,c],[d,e,f],[g,h,i]] else Finished Draw   
	


nextPlaya :: Player -> Player
nextPlaya X = O
nextPlaya O = X 




prettyPrint :: Game -> String
prettyPrint (board,player, _) = "The current player is " ++ show player ++ "\n" ++ unlines (comWhole board)
    where
        --square to character
        quare :: Square -> Char
        quare (Full X ) = 'X'
        quare (Full O) = 'O'
        quare Empty = ' '

        --outcome is a small board
        smallBo :: SmallBoard -> [String]
        smallBo (Finished( Win playa )) = replicate 3 (replicate 3 (bob playa))
            where
                bob X = 'X'
                bob O = 'O'
        smallBo (Finished Draw) = replicate 3 (replicate 3 'd')
        smallBo (UnFinished squares) = map (map quare) squares

        --three small boards left to right
        comRow :: [SmallBoard] -> [String]
        comRow [a,b,c] = comLines (smallBo a) (smallBo b) (smallBo c)
            where
                comLines :: [[Char]] -> [[Char]] -> [[Char]] -> [String]
                comLines [][][] = []
                comLines (j:j1) (h:h2) (k:k3) = (j ++ "|" ++ h ++ "|" ++ k): comLines j1 h2 k3
                comLines _ _ _ = error "all small boards must have same amount of rows"
        comRow _ = error "each row of big boards must have 3 small ones"

        --combine whole thing (all rows on top of each other)
        comWhole [j,h,k] = r1 ++ r2 ++ r3
            where
                r1 = comRow j
                r2 = comRow h
                r3 = comRow k
        comWhole _ = error "must be three by three"

gameStartSB = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]
gameStartGB = replicate 3 (replicate 3 gameStartSB)

checkWinner :: Game -> Maybe Winner
checkWinner (game, _, _) = bigBoardWin game
  where
    bigBoardWin :: GameBoard -> Maybe Winner
    bigBoardWin [[a,b,c], [d,e,f], [g,h,i]] =
        let
            -- Extract winner of a sub-board if it's finished
            eval :: SmallBoard -> Maybe Player
            eval (Finished (Win p)) = Just p
            eval _                  = Nothing

            squares =
                [ [a,b,c], [d,e,f], [g,h,i]     -- rows
                , [a,d,g], [b,e,h], [c,f,i]     -- columns
                , [a,e,i], [c,e,g]              -- diagonals
                ]

            getVals = map eval

            three p = [Just p, Just p, Just p]

            boardFull =
                all (\s -> case s of Finished _ -> True; _ -> False)
                    [a,b,c,d,e,f,g,h,i]

        in case () of
            _ | any (\l -> getVals l == three X) squares
                  -> Just (Win X)

              | any (\l -> getVals l == three O) squares
                  -> Just (Win O)

              | not boardFull
                  -> Nothing

              | otherwise
                  -> Just Draw

legalMoves :: Game -> [Move]
legalMoves (board, _, (_, sq)) = if checkSB sq board then [(x,y) | (x,y) <- moves, x == sq, checkSQ y (getSB sq board)] else [(x,y) | (x,y) <- moves, checkSB x board, checkSQ y (getSB x board)]
    where 

    checkSQ :: Int -> SmallBoard -> Bool
    checkSQ x (UnFinished [[a,b,c],[d,e,f],[g,h,i]])
      | x == 1 && a == Empty = True
      | x == 2 && b == Empty = True
      | x == 3 && c == Empty = True
      | x == 4 && d == Empty = True
      | x == 5 && e == Empty = True
      | x == 6 && f == Empty = True
      | x == 7 && g == Empty = True
      | x == 8 && h == Empty = True
      | x == 9 && i == Empty = True
      | otherwise                       	     = False


    getSB :: Int -> GameBoard -> SmallBoard
    getSB 1 [[a,b,c],[d,e,f],[g,h,i]] = a
    getSB 2 [[a,b,c],[d,e,f],[g,h,i]] = b
    getSB 3 [[a,b,c],[d,e,f],[g,h,i]] = c
    getSB 4 [[a,b,c],[d,e,f],[g,h,i]] = d
    getSB 5 [[a,b,c],[d,e,f],[g,h,i]] = e
    getSB 6 [[a,b,c],[d,e,f],[g,h,i]] = f
    getSB 7 [[a,b,c],[d,e,f],[g,h,i]] = g
    getSB 8 [[a,b,c],[d,e,f],[g,h,i]] = h
    getSB 9 [[a,b,c],[d,e,f],[g,h,i]] = i


    moves = [(x,y) | x <- [1..9], y <- [1..9]] 

    checkSB :: Int -> GameBoard -> Bool
    checkSB 1 [[(UnFinished _),b,c],[d,e,f],[g,h,i]] = True
    checkSB 2 [[a,(UnFinished y),c],[d,e,f],[g,h,i]] = True
    checkSB 3 [[a,b,(UnFinished y)],[d,e,f],[g,h,i]] = True
    checkSB 4 [[a,b,c],[(UnFinished y),e,f],[g,h,i]] = True
    checkSB 5 [[a,b,c],[d,(UnFinished y),f],[g,h,i]] = True
    checkSB 6 [[a,b,c],[d,e,(UnFinished y)],[g,h,i]] = True
    checkSB 7 [[a,b,c],[d,e,f],[(UnFinished y),h,i]] = True
    checkSB 8 [[a,b,c],[d,e,f],[g,(UnFinished y),i]] = True
    checkSB 9 [[a,b,c],[d,e,f],[g,h,(UnFinished y)]] = True
    checkSB _ _ = False
  
     

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

-- story 11 (not 3x3 to make it easier)
-- X -- current player
-- (0,0) -- previous move
-- 000 000 000 -- smallboard 1
-- 000 000 000
-- 000 000 000
-- 000 000 000
-- 000 000 000
-- 000 000 000
-- 000 000 000
-- 000 000 000
-- 000 000 000 -- smallboard 9

-- story 12
readGame :: String -> Game --main 
readGame str = (board, player, premove)
    where 
        ls = lines str
        player = case head ls of
            "X" -> X
            "O" -> O
            _   -> error "Invalid player"
        premove = read (head (drop 1 ls)) :: Move
        boardLines = drop 2 ls
        boards = [ take 3 boardLines, take 3 (drop 3 boardLines), take 3 (drop 6 boardLines)]
        board = [ map parseSmallBoard row | row <- boards ]
--helpers
parseSmallBoard :: String -> SmallBoard
parseSmallBoard st =
    let s = filter (/= ' ') st -- remove spaces
    in if length s /= 9 then error ("Smallboard must have 9 chars: " ++ st)
    else if all (== 'X') s then Finished (Win X)
    else if all (== 'O') s then Finished (Win O)
    else if any (== '0') s then UnFinished (parseSquares s)
    else Finished Draw

parseSquares :: String -> [[Square]]
parseSquares sq =
    let (row1, rest) = splitAt 3 sq
        (row2, row3) = splitAt 3 rest
    in [ map charToSquare row1, map charToSquare row2, map charToSquare row3]

charToSquare :: Char -> Square
charToSquare 'X' = Full X
charToSquare 'O' = Full O
charToSquare '0' = Empty
charToSquare _   = error "Invalid square character"
--end story 12

-- story 13
showGame :: Game -> String
showGame (board, player, premove) =
    let playerStr  = show player             -- cleaner than case
        premoveStr = show premove
        boardStrs  = map showSmallBoard (concat board)  -- [String]
    in unlines (playerStr : premoveStr : boardStrs)
--helper
showSmallBoard :: SmallBoard -> String
showSmallBoard (Finished (Win X)) = "XXX XXX XXX"
showSmallBoard (Finished (Win O)) = "OOO OOO OOO"
showSmallBoard (Finished Draw)   = "DDD DDD DDD" -- D for draw?? unless we change the Draw data type
showSmallBoard (UnFinished squares) =
    let [row1,row2,row3] = squares
    in unwords [map squareToChar row1, map squareToChar row2, map squareToChar row3]
  where
    squareToChar (Full X) = 'X'
    squareToChar (Full O) = 'O'
    squareToChar Empty    = '0'
-- end story 13
