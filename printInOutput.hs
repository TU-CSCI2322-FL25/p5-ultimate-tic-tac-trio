module printInOutput
( prettyPrint
, readGame
, showGame
) where

  import CoreGame
  
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
  
