module MainFxs
  ( checkFinalPosition
  , checkInitialPosition
  , checkWinner
  , fillBoard
  , makeMove
  , match
  , modifyBoard
  , noMoves
  , parseConfig
  , parseInput
  , score
  , p2h
  , h2p
  , rBoard
  , checkGameStatus
  , boardToMap
  , restructureGame
  , destructureGame
  ) where

import Data.Char
  ( digitToInt
  , isDigit
  )
import DataTypes
  ( Block
  , Board(..)
  , Hexx(..)
  , Hexagon(..)
  , Move(..)
  , Position(..)
  , Player(..)
  , GameInfo(..)
  , GameState(..)
  , RunGame(..)
  )
import UtilityFxs
  ( distance
  , getNearbyPositions
  )
import PlutusTx.AssocMap qualified as Map
import  PlutusLedgerApi.V2
  ( POSIXTime
  )
import Data.List (isPrefixOf)

checkGameStatus :: POSIXTime -> GameInfo -> Maybe RunGame
checkGameStatus epocht (GameInfo ps _ (Game _ dl b)) =
  case checkWinner b of
  Just (Red  , _) -> Just . GameOver $ h2p ps Red
  Just (Blue , _) -> Just . GameOver $ h2p ps Blue
  Just (Empty, _) -> Just Draw
  _ -> if epocht > dl then Just TimeOut else Nothing

p2h :: Player -> Hexagon
p2h p = case p of
  RedPlayer _ _ -> Red
  _ -> Blue

h2p :: [Player] -> Hexagon -> Player
h2p ps p = case p of
  Red  -> case head ps of
    RedPlayer  cs tn -> RedPlayer  cs tn
    _ -> last ps
  _    -> case head ps of
    BluePlayer cs tn -> BluePlayer cs tn
    _ -> last ps

playerToHex :: Player -> Hexagon
playerToHex p = case p of
  RedPlayer _ _ -> Red
  _             -> Blue

checkFinalPosition :: Board -> Move -> Maybe (Position, Integer)
checkFinalPosition (Board mph) m@(Move _ fp) = case Map.lookup fp mph of
  Just Empty -> case distance m of
    1 -> Just (fp, 1)
    2 -> Just (fp, 2)
    _ -> Nothing
  _ -> Nothing

checkInitialPosition :: Hexx -> Position -> Maybe Position
checkInitialPosition (Hexx opt pt lm w ob b@(Board mph)) ip = case Map.lookup ip mph of
  Just h  -> if   h == pt && length (concat $ getNearbyPositions b ip Empty <$> [1,2]) > 0
             then Just ip
             else Nothing
  _ -> Nothing

checkWinner :: Board -> Maybe (Hexagon, Board)
checkWinner b@(Board mph)
  | not . elem Blue $ Map.elems mph = Just (Red, b)
  | not . elem Red  $ Map.elems mph = Just (Blue, b)
  | noMoves Red b = case (score $ completeBoard Blue b) of
        (r, bl) | r > bl -> Just (Red, completeBoard Blue b)
                | r < bl -> Just (Blue, completeBoard Blue b)
                | otherwise -> Just (Empty, completeBoard Blue b)
  | noMoves Blue b = case (score $ completeBoard Red b) of
        (r, bl) | r > bl -> Just (Red, completeBoard Red b)
                | r < bl -> Just (Blue, completeBoard Red b)
                | otherwise -> Just (Empty, completeBoard Red b)
  | otherwise = Nothing

completeBoard :: Hexagon -> Board -> Board
completeBoard h b = case noMoves h b of
  False -> completeBoard h $ fillBoard h b
  _     -> b

fillBoard :: Hexagon -> Board -> Board
fillBoard h b@(Board mph) = Board . foldr (\x -> Map.insert x h) mph . concat $ (\x -> getNearbyPositions b x Empty 2) <$> (Map.keys $ Map.filter (== h) mph)

makeMove :: Hexx -> Move -> Integer -> Maybe Hexx
makeMove (Hexx opt pt lm w ob b@(Board mph)) (Move ip fp) d = case d of
  1 -> Just $ Hexx opt (succ pt) lm Nothing ob $
       Board $ foldr
       (\p -> Map.insert p pt)
       (Map.insert fp pt mph)
       (getNearbyPositions b fp (succ pt) 1)
  2 -> Just $ Hexx opt (succ pt) lm Nothing ob $
       Board $ foldr
       (\p -> Map.insert p pt)
       (Map.insert fp pt $ Map.insert ip Empty mph)
       (getNearbyPositions b fp (succ pt) 1)
  _ -> Nothing

match :: String -> String -> Bool
match s s' = isPrefixOf (head $ words s) s'

modifyBoard :: Board -> [Either Position Block] -> Board
modifyBoard b mods = Board $ foldr modifyBoard' b' mods
   where
     (Board b') = b
     modifyBoard' (Left   position) = Map.delete position
     modifyBoard' (Right (pos,hex)) = Map.insert pos hex

noMoves :: Hexagon -> Board -> Bool
noMoves h b@(Board mph) = null . concat . concat $ (\x -> getNearbyPositions b x Empty <$> [1,2]) <$> (Map.keys $ Map.filter (== h) mph)

parseConfig :: [String] -> [(Integer,Integer)]
parseConfig = fmap (\s -> (toInteger . digitToInt $ head s, toInteger . digitToInt $ last s)) <$> filter (\s -> all (== True) (fmap isDigit s) && length s == 2)

parseInput :: String -> Maybe Position
parseInput s = case length $ words s of
  1 -> case s of
       (x:y:[]) ->
         if   all isDigit [x] && all isDigit [y]
         then Just $ Position (read [x]) (read [y])
         else Nothing
       _ -> Nothing
  2 -> case words s of
       [x,y] ->
         if   all isDigit x && all isDigit y
         then Just $ Position (read x) (read y)
         else Nothing
       _ -> Nothing
  _ -> Nothing

rBoard :: [((Integer, Integer), Char)] -> Board
rBoard iic = Board . Map.fromList $ (\((x,y), c') -> (Position x y, charToHex c')) <$> iic
  where
    charToHex c' = case c' of
      'r' -> Red
      'b' -> Blue
      _   -> Empty

score :: Board -> (Int, Int)
score (Board b) = (length . Map.toList $ Map.filter (== Red) b, length . Map.toList $ Map.filter (== Blue) b)

boardToMap :: Board -> Map.Map Position Hexagon                                     
boardToMap (Board mph) = mph

restructureGame :: (Char, [((Integer, Integer), Char)]) -> (Hexagon, Board)
restructureGame (c, iic) = (charToHex c, Board . Map.fromList $ (\((x,y), c') -> (Position x y, charToHex c')) <$> iic)
  where                                              
    charToHex c' = case c' of                                        
      'r' -> Red                                             
      'b' -> Blue                                           
      _   -> Empty  

destructureGame :: (Hexagon, Board) -> (Char, [((Integer, Integer), Char)])
destructureGame (h, Board mph) = (hexToChar h, (\(p,h') -> ((getX p, getY p), hexToChar h')) <$> Map.toList mph)
  where
    hexToChar h' = case h' of 
      Red   -> 'r'
      Blue  -> 'b'
      Empty -> 'e'
