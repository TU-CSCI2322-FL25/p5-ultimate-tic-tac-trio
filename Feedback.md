Functionality (58+3=61/73 points)
* Game mechanics:                                 18/20 points
   * No files included for me to test against!
* Exact game solver:                                 15 points
* Cut-off depth solver:                                 12 points
* Reasonable evaluation function:                 1/2 points
   * You only count small boards that are adjacent in the concatenated list.
* Avoiding unnecessary work:                            3 points
   * Done, but very janky. 
* Command-line interface:                         6/10 points
   * prints the game on default/depth, instead of printing a move. you need both the -d and -w flag to get default behavior, which s shouldn’t work together.
   * Should stop after printing help.
   * Checks for -d and -w, but also requires both to run.
* Move and verbose flags:                         3/5 points
   * move flag works, verbose kinda of works (see CLI). Should not call whoWillWin, but get the rating from whoMightWin when the depth flag is passed
* Error-handling:                                 0/5 points
   * invalid moves errors instead of exiting cleanly.
   * checkFinished/bigBoardWin has refutable pattern matching
   * no error checking on addMove or bestMove 
* Makefile:                                        0/1 point
   * Doesn’t make! printInOutput.hs has the wrong name.
* Interactive                                        3/5
   * Doesn’t start with empty game if no file provided
   * Doesn’t show the forced position or legal moves.
Design (19/27 points)
* Well-designed data types                        8 points
* Well-decomposed functions                        5/10 points
   * optFile can never be set.
   * some very long lines.
   * checkWinner is just bigBoardWin with an extra bit of nesting?
   * result/terminal should just be a case expression on checkWinner IN whoWillWin. 
* Good module decomposition                        2 points
* Good variable names                                2 points
* Efficient/idiomatic code                        2/5 points
   * updateBoard/square is particularly icky. 
   * checkWinner is more convoluted than it needs to be.
   * calling whoWillWin MULTIPLE times in bestMove.
