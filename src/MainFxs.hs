module MainFxs 
  ( checkFinalPosition
  , checkInitialPosition
  , checkWinner
  , destructureGame
  , fillBoard
  , makeMove
  , match
  , modifyBoard
  , noMoves
  , parseConfig
  , parseInput
  , restructureGame
  , score
  ) where

import Data.Char 
  ( digitToInt
  , isDigit
  , toLower
  ) 
import DataTypes 
  ( Block
  , Board(..)
  , Hexx(..)
  , Hexagon(..)
  , Move(..)
  , Position(..)
  , Player(..)
  )
import UtilityFxs 
  ( distance
  , getNearbyPositions
  )
import PlutusTx.AssocMap qualified as Map

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
checkInitialPosition (Hexx pt lm w b@(Board mph)) ip = case Map.lookup ip mph of
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

makeMove :: Hexx -> Integer -> Maybe Hexx
makeMove (Hexx pt (Move ip fp) w b@(Board mph)) d = case d of
  1 -> Just $ Hexx (succ pt) (Move ip fp) Nothing $
       Board $ foldr 
       (\p -> Map.insert p pt) 
       (Map.insert fp pt mph)
       (getNearbyPositions b fp (succ pt) 1)
  2 -> Just $ Hexx (succ pt) (Move ip fp) Nothing $
       Board $ foldr 
       (\p -> Map.insert p pt) 
       (Map.insert fp pt $ Map.insert ip Empty mph)
       (getNearbyPositions b fp (succ pt) 1)
  _ -> Nothing

match :: String -> String -> Bool
match s s' = (toLower <$> (head $ words s)) == s' || (toLower <$> (head $ words s)) == [head s']

modifyBoard :: Board -> [Either Position Block] -> Board
modifyBoard b mods = Board $ foldr modifyBoard' b' mods
   where                                                                           
     (Board b') = b
     modifyBoard' (Left   position)   = Map.delete position                                                      
     modifyBoard' (Right (pos,hex))   = Map.insert pos hex 

noMoves :: Hexagon -> Board -> Bool
noMoves h b@(Board mph) = null . concat $ (\x -> getNearbyPositions b x Empty <$> [1,2]) <$> (Map.keys $ Map.filter (== h) mph)

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

destructureGame :: Hexx -> (Char, ((Integer, Integer), (Integer, Integer)), Char, [((Integer, Integer), Char)])
destructureGame (Hexx pt lm w (Board mph)) = 
  ( toChar pt
  , (\m -> (((\ip -> (getX ip, getY ip)) $ initialPosition m), ((\fp -> (getX fp, getY fp)) $ finalPosition m))) lm
  , parseWin w
  , (\(p, c) -> ((getX p, getY p), toChar c)) <$> Map.toList mph
  )
    where
      parseWin w = case w of
        Just Red   -> 'r'
        Just Blue  -> 'b'
        Just Empty -> 'e'
        _ ->          'n'
      toChar h = case h of 
        Red  -> 'r'
        Blue -> 'b'
        _    -> 'e'

restructureGame :: (Char, ((Integer, Integer), (Integer, Integer)), Char, [((Integer, Integer), Char)]) -> Hexx
restructureGame (pt, lm, w, b) = 
  Hexx
    (toHex pt)
    ((\((ipx, ipy), (fpx, fpy)) -> Move (Position ipx ipy) (Position fpx fpy)) lm)
    (parseWin w ) 
    (Board . Map.fromList $ (\((x,y), h) -> (Position x y, toHex h)) <$> b)
      where
        parseWin w = case w of
          'r' -> Just Red
          'b' -> Just Blue
          'e' -> Just Empty
          _   -> Nothing
        toHex c = case c of
          'r' -> Red
          'b' -> Blue
          _   -> Empty

score :: Board -> (Int, Int)
score (Board b) = (length . Map.toList $ Map.filter (== Red) b, length . Map.toList $ Map.filter (== Blue) b)
