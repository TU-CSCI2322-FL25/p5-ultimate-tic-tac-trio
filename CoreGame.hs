module CoreGame
( Player(..), Winner(..), Square(..)
, SmallBoard(..), GameBoard, Game
, Move
, nextPlaya
, addMove
, legalMoves
, checkWinner
, gameStartSB, gameStartBB, gameStart
) where



  data Player = X | O deriving (Show, Eq)
  -- helper function to change player 
  
  data Winner = Win Player | Draw deriving (Eq, Show)
  
  data Square = Full Player | Empty deriving (Eq, Show)
  
  type GameBoard = [[SmallBoard]]
  
  type Game = (GameBoard, Player, Move) 
  
  data SmallBoard = UnFinished [[Square]] | Finished Winner deriving (Eq, Show)
  
  type Move = (Integer, Integer)
  
  
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
  
  
  	updateSquare :: SmallBoard -> Integer -> Player -> SmallBoard
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
  
  
  gameStartSB = UnFinished [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]
  gameStartBB = replicate 3 (replicate 3 gameStartSB)
  gameStart = (gameStartBB, X, (0,0))

  checkWinner :: Game -> Maybe Winner
  checkWinner (game, _, _) = bigBoardWin game
    where
    bigBoardWin :: GameBoard -> Maybe Winner
    bigBoardWin [[a,b,c], [d,e,f], [g,h,i]] =
      let
        eval (Finished (Win p)) = Just p
        eval _                  = Nothing

        linesOfBoards =
            [ [a,b,c], [d,e,f], [g,h,i]
            , [a,d,g], [b,e,h], [c,f,i]
            , [a,e,i], [c,e,g]
            ]

        winnerFor p = any (all (== Just p) . map eval) linesOfBoards

        boardFull =
            all (\s -> case s of Finished _ -> True; _ -> False)
                [a,b,c,d,e,f,g,h,i]

      in case () of
          _ | winnerFor X -> Just (Win X)
            | winnerFor O -> Just (Win O)
            | not boardFull -> Nothing
            | otherwise -> Just Draw

  legalMoves :: Game -> [Move]
  legalMoves (board, _, (_, sq)) = if checkSB sq board then [(x,y) | (x,y) <- moves, x == sq, checkSQ y (getSB sq board)] else [(x,y) | (x,y) <- moves, checkSB x board, checkSQ y (getSB x board)]
    where 

    checkSQ :: Integer -> SmallBoard -> Bool
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


    getSB :: Integer -> GameBoard -> SmallBoard
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

    checkSB :: Integer -> GameBoard -> Bool
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
  
     

