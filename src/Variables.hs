module  Variables   ( classicBoard_S9DC3
                    ) where

import  DataTypes   (Board, Position (Position))
import  UtilityFxs  (makeStartingBoard)

-- A Classic 9 by 9 board with 3 centered deletions
classicBoard_S9DC3 :: Board
classicBoard_S9DC3 = makeStartingBoard 9 [Position 4 5, Position 5 4, Position 6 6]