data Player = PlayerX | PlayerO deriving (Show, Eq)
-- helper function to change player 

data Winner = Win Player | Draw | OnGoing deriving (Eq, Show)

data Square = X | O | Empty deriving (Eq, Show)

type GameBoard = [[Sqaure]]

type smallBox = [[Sqaure]]

type bigBox = [[smallBox]]

type location = (int, int) 

data prevMove = location
--helper function to change prev move and keep track like the player helper function





