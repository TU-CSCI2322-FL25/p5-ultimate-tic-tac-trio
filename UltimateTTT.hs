data Player = X | O deriving (Show, Eq)
-- helper function to change player 

data Winner = Win Player | Draw | OnGoing deriving (Eq, Show)

data Square = Full Player | Empty deriving (Eq, Show)

type GameBoard = [[SmallBoard]]

type Game = (GameBoard, Player) 

data SmallBoard = UnFinished [[Sqaure]] | Finished Winner deriving (Eq, Show)

type Loc = (Int, Int)

data prevMove = location


--helper function to change prev move and keep track like the player helper function





