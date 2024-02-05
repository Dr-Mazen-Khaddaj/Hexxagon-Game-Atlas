{-# OPTIONS_GHC -Wno-type-defaults #-}

module  Constants   ( classicBoard_S9DC3
                    , thousand
                    , million
                    , billion
                    ) where

import  DataTypes   (Board, Position (Position))
import  UtilityFxs  (makeStartingBoard)

-- A Classic 9 by 9 board with 3 centered deletions
classicBoard_S9DC3 :: Board
classicBoard_S9DC3 = makeStartingBoard 9 [Position 4 5, Position 5 4, Position 6 6]

thousand, million, billion  :: Num n => n
thousand  =  10 ^ 3
million   =  10 ^ 6
billion   =  10 ^ 9