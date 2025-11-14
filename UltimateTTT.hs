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

addMove :: Game -> Move -> Game
addMove (board, playa) move = (newB, nextPlaya playa)
where
    newB = updateB board move playa

nextPlaya :: Player -> Player
nextPlaya X = O
nextPlaya O = X 

-- replace element at index x with a new value
replaceAt :: Int -> a -> [a] -> [a]
replaceAt x new xs = 
    [if spot == x then new else y 
    | (spot,y) <- zip [0..] xs]

--get a column, duh
getColumn :: Int -> [[Square]] -> [Square]
getColumn c rs = 
    [sq | r <- rs, (cIndex,sq) <- zip [0..] r, cIndex == c]

-- see if small board had a winner (draw or ongoing)
checkSmallB :: [[Square]] -> Winner
checkSmallB rs 
    |any (all isX) rs = Win X
    |any (all isO) rs = Win O
    |any (all isO) cs = Win O
    |any (all isX) cs = Win X 
    |all(all isFilled) rs = Draw
    |otherwise = OnGoing
    where
        cs = [getColumn c rs | c <- [0..2]]
        --below checks square content
        isX (Full X) = True
        isX _ = False
        isO (Full O) = True
        isO _ = False
        isFilled Empty = False
        isFilled _ = True

--with the new move update the small board
updateSmallB :: SmallBoard -> Loc -> Player -> SmallBoard
updateSmallB (Finished w) _ _ = Finished w 
updateSmallB (UnFinished rs) (r,c) playa = 
    case winner of
        OnGoing -> UnFinished newRs
        w -> Finished w 
    where
        newRs = [if rIndex == r 
            then replaceAt c (Full playa) row 
            else row 
            | (rIndex, row) <- zip[0..]rs]
        winner = checkSmallB newRs

updateRo :: [SmallBoard] -> Int -> Loc -> Player -> [SmallBoard]
updateRo r bigC smLoc playa = 
    [if cIndex == bigC 
        then updateSmallB sB smLoc playa 
        else sB 
        | (cIndex,sB) <- zip[0..] r]

updateB :: GameBoard -> Move -> Player -> GameBoard
updateB b ((bigRow, bigCol), smLoc) playa =
    [if rIndex == bigRow 
        then updateRo r bigCol smLoc playa 
        else r 
        | (rIndex,r) <- zip [0..] b]



prettyPrint :: Game -> String
prettyPrint (board,player) = "The current player is " ++ show player ++ "\n" ++ unlines (comBig board)
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
checkWinner (game, _) = bigBoardWin (map (map smallBoardWin) game)
	where 
	smallBoardWin :: SmallBoard -> SmallBoard
	smallBoardWin (UnFinished [[a,b,c], [d,e,f], [g,h,i]]) =
		let squares = [ [a,b,c], [d,e,f], [g,h,i], [a,d,g], [b,e,h], [c,f,i], [a,e,i], [c,e,g] ]
		in if any (all (== Full X)) squares then Finished (Win X) else if any (all (== Full O)) squares then Finished (Win O) else UnFinished [[a,b,c],[d,e,f],[g,h,i]]
	smallBoardWin sb@(Finished _) = sb

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




