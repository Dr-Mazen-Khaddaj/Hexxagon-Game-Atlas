module  UtilityFxs  ( distance
                    ) where

import DataTypes (Move (..), Position (..))

distance :: Move -> Int
distance m = abs deltaX + abs deltaY + abs (deltaX - deltaY)
    where
        Position xi yi = m.initialPosition
        Position xf yf = m.finalPosition
        deltaX = xf - xi
        deltaY = yf - yi