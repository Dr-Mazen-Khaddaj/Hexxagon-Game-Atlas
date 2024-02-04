module MainFxs (checkFP, checkIP, checkWinner, fillBoard, makeMove, noMoves) where

import Data.Map qualified as Map
import DataTypes (Board(..), GameInfo(..), GameState(..), Hexagon(..), Move(..), Player(..), Position)
import UtilityFxs (distance, getNearbyPositions)

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

makeMove :: Move -> GameInfo -> Maybe GameState
makeMove m@(Move ip fp) (GameInfo ps _ gs@(Game _ dl b@(Board mph))) = case (checkIP ip gs, checkFP m b) of
  (Just (p, h), Just 2) -> Just $ Game (head $ filter (/= p) ps) dl $ 
                           Board $ foldr 
                           (\x -> Map.insert x h) 
                           (Map.insert fp h $ Map.delete ip mph)
                           (getNearbyPositions b fp (if h == Red then Blue else Red) 1)
  (Just (p, h), Just 1) -> Just $ Game (head $ filter (/= p) ps) dl $ 
                           Board $ foldr 
                           (\x -> Map.insert x h) 
                           (Map.insert fp h mph)
                           (getNearbyPositions b fp (if h == Red then Blue else Red) 1)
  _ -> Nothing


noMoves :: Hexagon -> Board -> Bool
noMoves h b@(Board mph) = null . concat $ (\x -> getNearbyPositions b x Empty 2) <$> (Map.keys $ Map.filter (== h) mph)

fillBoard :: Hexagon -> Board -> Board
fillBoard h b@(Board mph) = Board . foldr (\x -> Map.insert x h) mph . concat $ (\x -> getNearbyPositions b x Empty 2) <$> (Map.keys $ Map.filter (== h) mph)

completeBoard :: Hexagon -> Board -> Board
completeBoard h b = case noMoves h b of  
  False -> completeBoard h $ fillBoard h b 
  _     -> b

checkWinner :: Board -> Maybe Hexagon
checkWinner b@(Board mph)
  | not . elem Blue $ Map.elems mph = Just Red
  | not . elem Red  $ Map.elems mph = Just Blue
  | noMoves Red b = case (score $ completeBoard Blue b) of
        (r, bl) | r > bl -> Just Red
                | r < bl -> Just Blue
                | otherwise -> Just Empty
  | noMoves Blue b = case (score $ completeBoard Red b) of
        (r, bl) | r > bl -> Just Red
                | r < bl -> Just Blue
                | otherwise -> Just Empty
  | otherwise = Nothing

score :: Board -> (Int, Int)
score (Board b) = (Map.size $ Map.filter (== Red) b, Map.size $ Map.filter (== Blue) b)



