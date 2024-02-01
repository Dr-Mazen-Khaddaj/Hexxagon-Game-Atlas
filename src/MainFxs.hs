module MainFxs (checkFP, checkIP, makeM) where

import Data.Map qualified as Map
import DataTypes (Board(..), GameInfo(..), GameState(..), Hexagon(..), Move(..), Player(..), Position)
import UtilityFxs (distance)

checkIP :: Position -> GameState -> Maybe (Player, Hexagon)
checkIP ip (Game pt _ (Board b)) = case Map.lookup ip b of
  Just h -> case pt of 
    RedPlayer  cs tn -> if h == Red  then Just (RedPlayer  cs tn, Red ) else Nothing
    BluePlayer cs tn -> if h == Blue then Just (BluePlayer cs tn, Blue) else Nothing
  _ -> Nothing

checkFP :: Move -> Board -> Maybe Int
checkFP m@(Move _ fp) (Board b) = case Map.lookup fp b of
  Just Empty -> case distance m of
    2 -> Just 2          
    1 -> Just 1
    _ -> Nothing
  _ -> Nothing

makeM :: Move -> GameInfo -> GameState
makeM m@(Move ip fp) (GameInfo ps _ gs@(Game _ dl b@(Board mph))) = case (checkIP ip gs, checkFP m b) of
  (Just (p, h), Just 2) -> Game (head $ filter (/= p) ps) dl . Board . Map.insert fp h $ Map.delete ip mph
  (Just (p, h), Just 1) -> Game (head $ filter (/= p) ps) dl . Board $ Map.insert fp h mph
  _ -> gs
