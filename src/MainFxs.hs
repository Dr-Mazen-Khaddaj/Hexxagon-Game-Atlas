module MainFxs (checkFinalPosition, checkInitialPosition, checkWinner, fillBoard, makeMove, noMoves, parseInput, score) where

import Data.Char (isDigit)
import DataTypes (Board(..), GameInfo(..), GameState(..), Hexagon(..), Move(..), Player(..), Position(..))
import PlutusTx.AssocMap qualified as AssocMap                              
import UtilityFxs (distance, getNearbyPositions)

parseInput :: String -> Maybe Position
parseInput s = case words s of
  [x,y] -> if all isDigit x && all isDigit y then Just $ Position (read x) (read y) else Nothing
  _ -> Nothing

checkInitialPosition :: GameState -> Position -> Maybe (Position, Player, Hexagon)
checkInitialPosition (Game pt _ (Board b)) ip = case AssocMap.lookup ip b of
  Just h -> case pt of 
    RedPlayer  cs tn -> if h == Red  then Just (ip, RedPlayer  cs tn, Red ) else Nothing
    BluePlayer cs tn -> if h == Blue then Just (ip, BluePlayer cs tn, Blue) else Nothing
  _ -> Nothing

checkFinalPosition :: Board -> Move -> Maybe (Position, Int)
checkFinalPosition (Board b) m@(Move _ fp) = case AssocMap.lookup fp b of
  Just Empty -> case distance m of
    2 -> Just (fp, 2)         
    1 -> Just (fp, 1)
    _ -> Nothing
  _ -> Nothing

makeMove :: GameInfo -> Position -> Player -> Hexagon -> Position -> Int -> Maybe GameState
makeMove (GameInfo ps _ (Game _ dl b@(Board mph))) ip p h fp d = case d of
  2 -> Just $ Game (head $ filter (/= p) ps) dl $ 
       Board $ foldr 
       (\x -> AssocMap.insert x h) 
       (AssocMap.insert fp h $ AssocMap.insert ip Empty mph)
       (getNearbyPositions b fp (if h == Red then Blue else Red) 1)
  1 -> Just $ Game (head $ filter (/= p) ps) dl $
       Board $ foldr 
       (\x -> AssocMap.insert x h) 
       (AssocMap.insert fp h mph)
       (getNearbyPositions b fp (if h == Red then Blue else Red) 1)
  _ -> Nothing

checkWinner :: Board -> Maybe (Hexagon, Board)
checkWinner b@(Board mph)
  | not . elem Blue $ AssocMap.elems mph = Just (Red, completeBoard Red b)
  | not . elem Red  $ AssocMap.elems mph = Just (Blue, completeBoard Blue b)
  | noMoves Red b = case (score $ completeBoard Blue b) of
        (r, bl) | r > bl -> Just (Red, completeBoard Blue b)
                | r < bl -> Just (Blue, completeBoard Blue b)
                | otherwise -> Just (Empty, completeBoard Blue b)
  | noMoves Blue b = case (score $ completeBoard Red b) of
        (r, bl) | r > bl -> Just (Red, completeBoard Red b)
                | r < bl -> Just (Blue, completeBoard Red b)
                | otherwise -> Just (Empty, completeBoard Red b)
  | otherwise = Nothing

noMoves :: Hexagon -> Board -> Bool
noMoves h b@(Board mph) = null . concat $ (\x -> getNearbyPositions b x Empty 2) <$> (AssocMap.keys $ AssocMap.filter (== h) mph)

completeBoard :: Hexagon -> Board -> Board
completeBoard h b = case noMoves h b of  
  False -> completeBoard h $ fillBoard h b 
  _     -> b

fillBoard :: Hexagon -> Board -> Board
fillBoard h b@(Board mph) = Board . foldr (\x -> AssocMap.insert x h) mph . concat $ (\x -> getNearbyPositions b x Empty 2) <$> (AssocMap.keys $ AssocMap.filter (== h) mph)

score :: Board -> (Int, Int)
score (Board b) = (length . AssocMap.toList $ AssocMap.filter (== Red) b, length . AssocMap.toList $ AssocMap.filter (== Blue) b)
