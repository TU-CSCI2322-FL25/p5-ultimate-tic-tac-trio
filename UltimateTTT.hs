data Player = X | O deriving (Show, Eq)
-- helper function to change player 

data Winner = Win Player | Draw | OnGoing deriving (Eq, Show)

data Square = Full Player | Empty deriving (Eq, Show)

type GameBoard = [[SmallBoard]]

type Game = (GameBoard, Player, Move) 

data SmallBoard = UnFinished [[Square]] | Finished Winner deriving (Eq, Show)

type Move = (Int, Int)


--helper function to change prev move and keep track like the player helper function

addMove :: Game -> Move -> Game
addMove (board, playa, premove) move = if move `elem` legalm then (newB, nextPlaya playa, move) else error "That is not a legal move punk >:("
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
	updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 1 p = [[Full p,b,c],[d,e,f],[g,h,i]] 
        updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 2 p = [[a,Full p,c],[d,e,f],[g,h,i]]
        updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 3 p = [[a,b,Full p],[d,e,f],[g,h,i]]
        updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 4 p = [[a,b,c],[Full p,e,f],[g,h,i]]
        updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 5 p = [[a,b,c],[d,Full p,f],[g,h,i]]
        updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 6 p = [[a,b,c],[d,e,Full p],[g,h,i]]
        updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 7 p = [[a,b,c],[d,e,f],[Full p,h,i]]
        updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 8 p = [[a,b,c],[d,e,f],[g,Full p,i]]
        updateSquare Unfinished [[a,b,c],[d,e,f],[g,h,i]] 9 p = [[a,b,c],[d,e,f],[g,h,Full p]]


	checkFinished :: SmallBoard -> SmallBoard
	checkFinished UnFinished [[a,b,c],[d,e,f],[g,h,i]] = 
		let wins = [[a,b,c],[d,e,f],[g,h,i],[a,d,g],[b,e,h],[c,f,i],[a,e,i],[c,e,g]]
		    squares = [a,b,c,d,e,f,g,h,i]
		if any(all(==Full X) wins then Finished Win X else if any (all (==Full O)) wins then Finished Win O else if any (==Empty) squares then Unfinished [[a,b,c],[d,e,f],[g,h,i]] else Finished Draw   
	
	nextPlaya :: Player -> Player
	nextPlaya X = O
	nextPlaya O = X 




prettyPrint :: Game -> String
prettyPrint (board,player, _) = "The current player is " ++ show player ++ "\n" ++ unlines (comBig board)
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
        r2 = comROw h
        r3 = comRow k
    comWhole _ = error "must be three by three"

gameStartSB = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]
gameStartGB = replicate 3 (replicate 3 gameStartSB)

checkWinner :: Game -> Winner
checkWinner (game, _) = bigBoardWin game
	bigBoardWin :: GameBoard -> Winner
	bigBoardWin [[a,b,c], [d,e,f], [g,h,i]] =
		let eval x = case x of Finished (Win X) -> Just X
		                       Finished (Win O) -> Just O
				       _                -> Nothing
		    squares = [ [a,b,c], [d,e,f], [g,h,i], [a,d,g], [b,e,h], [c,f,i], [a,e,i], [c,e,g] ]
		    
                    getVals line = map eval line
  
                in if any (\l -> getVals l == [Just X, Just X, Just X]) squares
			then Win X
		        else if any (\l -> getVals l == [Just O, Just O, Just O]) squares
				then Win O
				else if any (\s -> case s of UnFinished _ -> True; _ -> False) [a,b,c,d,e,f,g,h,i]
					then OnGoing 
					else Draw

legalMoves :: Game -> [Move]
legalMoves (board, _, (_, sq)) = if checkSB sq board then [(x,y) | x <- moves, x == sq, checkSQ y sq] else [(x,y) | x <- moves, checkSQ y (getSB x board)]
    where 

    checkSQ :: Int -> SmallBoard -> Bool
    checkSQ x [[a,b,c],[d,e,f],[g,h,i]]
      | 1 [[Empty,b,c],[d,e,f],[g,h,i]] = True
      | 2 [[a,Empty,c],[d,e,f],[g,h,i]] = True
      | 3 [[a,b,Empty],[d,e,f],[g,h,i]] = True
      | 4 [[a,b,c],[Empty,e,f],[g,h,i]] = True
      | 5 [[a,b,c],[d,Empty,f],[g,h,i]] = True
      | 6 [[a,b,c],[d,e,Empty],[g,h,i]] = True
      | 7 [[a,b,c],[d,e,f],[Empty,h,i]] = True
      | 8 [[a,b,c],[d,e,f],[g,Empty,i]] = True
      | 9 [[a,b,c],[d,e,f],[g,h,Empty]] = True
      | Otherwise                       = False


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
    checkSB x [[a,b,c],[d,e,f],[g,h,i]] 
      | 1 [[UnFinished _,b,c],[d,e,f],[g,h,i]] = True
      | 2 [[a,UnFinished _,c],[d,e,f],[g,h,i]] = True
      | 3 [[a,b,UnFinished _],[d,e,f],[g,h,i]] = True
      | 4 [[a,b,c],[UnFinished _,e,f],[g,h,i]] = True
      | 5 [[a,b,c],[d,UnFinished _,f],[g,h,i]] = True
      | 6 [[a,b,c],[d,e,Unfinished _],[g,h,i]] = True
      | 7 [[a,b,c],[d,e,f],[UnFinished _,h,i]] = True
      | 8 [[a,b,c],[d,e,f],[g,UnFinished _,i]] = True
      | 9 [[a,b,c],[d,e,f],[g,h,UnFinished _]] = True
      | Otherwise                              = False
  
     













