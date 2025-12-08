module PrintInOutput (prettyPrint, readGame, showGame) 
  
  where

  import CoreGame
  
  prettyPrint :: Game -> String
  prettyPrint (board, player, _) = "The current player is " ++ show player ++ "\n" ++ unlines (comWhole board)
    where
    -- square to character
    squareChar :: Square -> Char
    squareChar (Full X) = 'X'
    squareChar (Full O) = 'O'
    squareChar Empty    = ' '

    -- output a small board as 3 strings
    smallBo :: SmallBoard -> [String]
    smallBo (Finished (Win p)) = replicate 3 (replicate 3 (playerChar p))
      where
        playerChar X = 'X'
        playerChar O = 'O'
    smallBo (Finished Draw) = replicate 3 (replicate 3 'd')
    smallBo (UnFinished squares) = map (map squareChar) squares

    -- combine three small boards horizontally
    comRow :: [SmallBoard] -> [String]
    comRow [a, b, c] = comLines (smallBo a) (smallBo b) (smallBo c)
      where
        comLines :: [String] -> [String] -> [String] -> [String]
        comLines [] [] [] = []
        comLines (r1:rs1) (r2:rs2) (r3:rs3) =
            (r1 ++ "|" ++ r2 ++ "|" ++ r3) : comLines rs1 rs2 rs3
        comLines _ _ _ = error "all small boards must have the same number of rows"
    comRow _ = error "each row of big board must have exactly 3 small boards"

    -- combine big board (3 rows of 3 small boards)
    comWhole :: [[SmallBoard]] -> [String]
    comWhole [r1, r2, r3] = comRow r1 ++ comRow r2 ++ comRow r3
    comWhole _ = error "big board must be 3×3"
	  
  
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
  
