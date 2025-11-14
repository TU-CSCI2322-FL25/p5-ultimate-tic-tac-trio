data Player = X | O deriving (Show, Eq)
-- helper function to change player 

data Winner = Win Player | Draw | OnGoing deriving (Eq, Show)

data Square = Full Player | Empty deriving (Eq, Show)

type GameBoard = [[SmallBoard]]

type Game = (GameBoard, Player) 

data SmallBoard = UnFinished [[Square]] | Finished Winner deriving (Eq, Show)

type Loc = (Int, Int)

data prevMove = location


--helper function to change prev move and keep track like the player helper function

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




