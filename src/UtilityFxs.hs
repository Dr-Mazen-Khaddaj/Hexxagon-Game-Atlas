module  UtilityFxs  ( distance
                    , getNearbyPositions
                    , makeEmptyBoard
                    , makeEmptyClassicBoard
                    , makeClassicBoard
                    , makeStartingBoard
                    ) where

import DataTypes (Move (..), Position (..), Board (Board), Hexagon (..), Block)
import qualified Data.Map as Map

----------------------------------------------------------------------------------------------------------------------------
-- Calculate distance of a move
distance :: Move -> Integer
distance m = (`div` 2) $ abs deltaX + abs deltaY + abs (deltaX - deltaY)
    where
        Position xi yi = m.initialPosition
        Position xf yf = m.finalPosition
        deltaX = xf - xi
        deltaY = yf - yi

-- Get nearby positions of a specific hexagon type
getNearbyPositions :: Board -> Position -> Hexagon -> Integer -> [Position]
getNearbyPositions (Board board) pos hex d = filter rightHex $ calculateNearbyPositions pos d
    where
        rightHex p = case Map.lookup p board of
            Just h  -> h == hex
            Nothing -> False

-- Calculate All possible nearby positions, given a starting position, and distance
calculateNearbyPositions :: Position -> Integer -> [Position]
calculateNearbyPositions (Position x y) d = [ Position (x + deltaX) (y + deltaY) | (deltaX, deltaY) <- calculateDeltas d]
    where
        calculateDeltas :: Integer -> [(Integer,Integer)]
        calculateDeltas n = concat [s1,s2,s3,s4,s5,s6]
            where
                s1 = (n,) <$> [0..n]
                s2 = (,n) <$> [0..n-1]
                s3 = (,-n) <$> [0,-1..(-n)]
                s4 = (-n,) <$> [0,-1..(-n+1)]
                s5 = [ (i,i-n) | i <- [1..n-1]]
                s6 = [ (i-n,i) | i <- [1..n-1]]

---------------------------------------------- | Board Generation Functions | ----------------------------------------------

-- Make a simple n by n sized empty board
makeEmptyBoard :: Integer -> Board
makeEmptyBoard size = Board $ Map.fromList [(Position x y , Empty) | x <- [1..size] , y <- [1..size]]

-- Make a classic n by n empty board (Hexagonal shaped board)
makeEmptyClassicBoard :: Integer -> Board
makeEmptyClassicBoard size = Board $ Map.fromList [(Position x y , Empty) | x <- [1..size] , y <- [1..size] , abs (x-y) <= size `div` 2]

-- Make a modified classic n by n board by passing modifications (deletions and insertions) as a second argument
makeClassicBoard :: Integer -> [Either Position Block] -> Board
makeClassicBoard size modifications = Board $ foldr modifyBoard board modifications
    where
        (Board board) = makeEmptyClassicBoard size
        modifyBoard (Left   position)   = Map.delete position
        modifyBoard (Right (pos,hex))   = Map.insert pos hex

-- Make a classic n by n board, with the starting positions filled with player units, and can be modified (Only by deletions)
makeStartingBoard :: Integer -> [Position] -> Board
makeStartingBoard size = makeClassicBoard size . (insertions <>) . (Left <$>)
    where
        insertions = Right <$> blueBlocks <> redBlocks
        blueBlocks = (,Blue) <$> [Position 1 1       , Position lowerMid size , Position size lowerMid ]
        redBlocks  = (,Red)  <$> [Position size size , Position 1 higherMid   , Position higherMid 1   ]
        lowerMid  = (size + 1) `div` 2
        higherMid = size `div` 2 + 1

----------------------------------------------------------------------------------------------------------------------------