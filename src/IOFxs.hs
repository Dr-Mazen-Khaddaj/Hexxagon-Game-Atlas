
module IOFxs where
import Control.Exception 
  ( IOException
  , try
  )
import Control.Monad.State
import DataTypes 
  ( Board(..)
  , Hexx(..)
  , Hexagon(..)
  , Move(..)
  , Position(..)
  , GameState(..)
  , RunGame(..)
  , GameInfo(..)
  , Player(..)
  )
import Instances 
  ( BoardMode(..)
  , Orientation(Edge)
  , showBoard
  )
import MainFxs 
  ( checkFinalPosition
  , checkInitialPosition
  , checkWinner
  , destructureGame
  , makeMove
  , match
  , modifyBoard
  , parseConfig
  , parseInput
  , restructureGame
  , score
  , h2p
  , p2h
  , rBoard
  )
import System.Console.ANSI 
  ( ColorIntensity(..)
  , ConsoleIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , clearScreen
  , setCursorPosition
  , setSGRCode
  )
import System.Console.Haskeline
import System.IO 
  ( hFlush
  , stdout
  )
import UtilityFxs 
  ( getNearbyPositions
  , makeStartingBoard
  , makeEmptyClassicBoard
  )
import PlutusTx.AssocMap qualified as Map
import qualified System.Console.ANSI as C (Color(..))
import Constants
  ( classicBoard_S9DC3
  )
import Data.Time.Clock.POSIX 
  ( getPOSIXTime
  )

playTurn :: GameInfo -> IO RunGame
playTurn (GameInfo ps td (Game pt dl b)) = do
  hexx <- hexxagon (p2h pt) b
  case hexx of
    Just (Hexx pt lm w oc b) -> do
      posixtime <- getPOSIXTime
      if   round posixtime * 1000 > dl 
      then return TimeOut 
      else
        case w of
          Just Red   -> return . GameOver $ h2p ps Red
          Just Blue  -> return . GameOver $ h2p ps Blue
          Just Empty -> return Draw
          _ -> return $ PlayTurn lm
    _ -> undefined


hexxagon :: Hexagon -> Board -> IO (Maybe Hexx)
hexxagon pt b = runInputT defaultSettings $ checkWinnerIO $ Hexx pt (Move (Position (-1) (-1)) (Position (-1) (-1))) Nothing True b

cBlue :: Char -> [Char]
cBlue c = setSGRCode [SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]

cRed :: Char -> [Char]
cRed  c = setSGRCode [SetColor Background Dull C.Red]  <> [c] <> setSGRCode [Reset]

fm = Move (Position (-1) (-1)) (Position (-1) (-1))

checkWinnerIO :: Hexx -> InputT IO (Maybe Hexx)
checkWinnerIO g@(Hexx pt lm w oc b) = case checkWinner b of
  Just (Red, b')   -> do screenWinner $ Hexx pt lm (Just Red) oc b'
                         if   oc == True
                         then return . Just $ Hexx pt lm (Just Red) oc b
                         else do
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO $ Hexx pt lm (Just Red) oc b'
                             Just s' | any (== True) $ match s' <$> ["load", "save", "quit"] -> loadSaveQuit s' g
                             _ -> checkWinnerIO $ Hexx pt lm (Just Red) oc b'
  Just (Blue, b')  -> do screenWinner $ Hexx pt lm (Just Blue) oc b'; _ <- return . Just $ Hexx pt lm w oc b'
                         if   oc == True
                         then return . Just $ Hexx pt lm (Just Blue) oc b
                         else do
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO $ Hexx pt lm (Just Blue) oc b'
                             Just s' | any (== True) $ match s' <$> ["load", "save", "quit"] -> loadSaveQuit s' g
                             _ -> checkWinnerIO $ Hexx pt lm (Just Blue) oc b'
  Just (Empty, b') -> do screenWinner $ Hexx pt lm (Just Empty) oc b'; _ <- return . Just $ Hexx pt lm w oc b'
                         if   oc == True
                         then return . Just $ Hexx pt lm (Just Empty) oc b 
                         else do
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO $ Hexx pt lm (Just Empty) oc b'
                             Just s' | any (== True) $ match s' <$> ["load", "save", "quit"] -> loadSaveQuit s' g
                             _ -> checkWinnerIO $ Hexx pt lm (Just Blue) oc b'
  _ -> do screen g
          if   oc == True && lm /= fm
          then return $ Just g
          else do
            s <- getInputLine "   "
            case s of
              Just [] -> game g
              Just s' | any (== True) $ match s' <$> ["load", "save", "quit"] -> loadSaveQuit s' g
              _ -> game g
  where
    loadSaveQuit s g_@(Hexx _ _ _ oc _)
      | match s "save" = do saveGame g s; checkWinnerIO g_
      | match s "load" = if oc == True then checkWinnerIO g_ else
        do g' <- loadGame s
           case g' of
             Just g'' -> do
               let g''' = read g'' :: (Char, ((Integer, Integer), (Integer, Integer)), Char, Char, [((Integer, Integer), Char)])
               checkWinnerIO $ restructureGame g'''
             _ -> checkWinnerIO g_
      | otherwise = return $ Just g_

colorize :: Char -> [Char]
colorize c
  | 'R' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Dull C.Red] <> [c] <> setSGRCode [Reset]
  | 'B' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]
  | 'E' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Black] <> [c] <> setSGRCode [Reset]
  | otherwise = [c]

hexBoardConfig :: IO ()
hexBoardConfig = configBoard $ Hexx Red (Move (Position (-1) (-1)) (Position (-1) (-1))) Nothing False $ makeEmptyClassicBoard 3

configBoard :: Hexx -> IO ()
configBoard g = runInputT defaultSettings $configBoard' g

configBoard' :: Hexx -> InputT IO ()
configBoard' (Hexx pt lm w oc b)  = do 
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge CoordsAndHexs [] b
  input <- getInputLine "   "
  case input of
    Just [] -> configBoard' (Hexx pt lm w oc b)
    Just s
      | match s "blue"   -> configBoard' $ Hexx pt lm w oc $ modifyBoard b $ (\(x,y) -> Right ((Position x y), Blue)) <$> (parseConfig . tail $ words s)
      | match s "delete" -> configBoard' $ Hexx pt lm w oc $ modifyBoard b $ (\(x,y) -> Left (Position x y)) <$> (parseConfig . tail $ words s)
      | match s "empty"  -> configBoard' $ Hexx pt lm w oc $ modifyBoard b $ (\(x,y) -> Right ((Position x y), Empty)) <$> (parseConfig . tail $ words s)
      | match s "load"   -> do g' <- loadGame s
                               case g' of
                                 Just g'' -> do
                                   let g''' = read g'' :: (Char, ((Integer, Integer), (Integer, Integer)), Char, Char, [((Integer, Integer), Char)])
                                   configBoard' $ restructureGame g'''
                                 _ -> configBoard' $ Hexx pt lm w oc b
      | match s "quit"   -> return ()
      | match s "red"    -> configBoard' $ Hexx pt lm w oc $ modifyBoard b $ (\(x,y) -> Right ((Position x y), Red)) <$> (parseConfig . tail $ words s)
      | match s "save"   -> do saveGame (Hexx pt lm (Just Red) oc b) s; configBoard' $ Hexx pt lm w oc b
    _ -> configBoard' $ Hexx pt lm w oc b

game :: Hexx -> InputT IO (Maybe Hexx)
game g@(Hexx pt lm w oc b) = do
  ip <- getInitialPosition g
  case ip of
    Right ip' -> case ip' of
      Just ip'' -> do
        fp <- getFinalPosition g ip''
        case fp of
          Right fp' -> case fp' of
            Just (fp'', d) -> do
              case makeMove (Hexx pt (Move ip'' fp'') w oc b) d of
                Just g' -> checkWinnerIO g'
                _ -> return Nothing
            _ -> checkWinnerIO g
          Left g' -> case g' of
            Just g'' -> checkWinnerIO g''
            _ -> return g' 
      _ -> checkWinnerIO g
    Left g' -> case g' of
      Just g'' -> checkWinnerIO g''
      _ -> return g'

getFinalPosition :: Hexx -> Position -> InputT IO (Either (Maybe Hexx) (Maybe (Position, Integer)))
getFinalPosition g@(Hexx pt lm w oc b) ip = do
  liftIO resetCursor
  if oc == True then hexxagonTitle_c pt else hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2]) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["load", "save", "quit"] -> posLoadSaveQuit s (flip getFinalPosition ip) g
    _ -> return . Right $ fp >>= parseInput >>= checkFinalPosition b . Move ip

getInitialPosition :: Hexx -> InputT IO (Either (Maybe Hexx) (Maybe Position))
getInitialPosition g@(Hexx pt lm w oc b@(Board mph)) = do
  liftIO resetCursor
  if oc == True then hexxagonTitle_c pt else hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge SelectiveCoords (filter (\p -> length (concat $ getNearbyPositions b p Empty <$> [1,2]) > 0) $ Map.keys $ Map.filter (== pt) mph) b
  ip <- getInputLine "   "
  case ip of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["load", "save", "quit"] -> posLoadSaveQuit s getInitialPosition g
    _ -> return . Right $ ip >>= parseInput >>= checkInitialPosition g

hexxagonTitle :: Hexagon -> InputT IO ()
hexxagonTitle h = 
  if   h == Red 
  then outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull C.Red  , SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  else outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]

hexxagonTitle_c :: Hexagon -> InputT IO ()
hexxagonTitle_c h = 
  if   h == Red 
  then outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull C.Red  , SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset] <> setSGRCode [SetConsoleIntensity BoldIntensity] <> " CARDANO " <> setSGRCode [Reset] <> "- Warning! Move is final and will be sent to Cardano blockchain for validation! Save game and load in a local Hexxagon game to practice from this state!"
  else outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset] <> setSGRCode [SetConsoleIntensity BoldIntensity] <> " CARDANO " <> setSGRCode [Reset] <> "- Warning! Move is final and will be sent to Cardano blockchain for validation! Save game and load in a local Hexxagon game to practice from this state!"

hexxagonWinner :: Maybe Hexagon -> InputT IO ()
hexxagonWinner w = case w of
  Just Red  -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull  C.Red ,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  Just Blue -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  _         -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN TIE! " <> setSGRCode [Reset]

loadGame :: String -> InputT IO (Maybe String)
loadGame s = do
  g <- liftIO . try . readFile . last $ words s :: InputT IO (Either IOException String)
  case g of
    Right g' -> return $ Just g'
    _        -> return Nothing

posLoadSaveQuit :: String -> (Hexx -> InputT IO (Either (Maybe Hexx) b)) -> Hexx -> InputT IO (Either (Maybe Hexx) b)
posLoadSaveQuit s f g@(Hexx _ _ _ oc _)
  | match s "save" = do saveGame g s; f g
  | match s "load" = if oc == True then f g else
    do g' <- loadGame s
       case g' of
         Just g'' -> do
           let g''' = read g'' :: (Char, ((Integer, Integer), (Integer, Integer)), Char, Char, [((Integer, Integer), Char)])
           return . Left . Just $ restructureGame g'''
         _ -> f g
  | otherwise = return . Left $ Nothing

resetCursor :: IO ()
resetCursor = do
  clearScreen
  setCursorPosition 0 0
  hFlush stdout

saveGame :: Hexx -> String -> InputT IO ()
saveGame g s = do
  g <- liftIO . try . writeFile (last $ words s) . show $ destructureGame g :: InputT IO (Either IOException ())
  case g of
    Right g' -> return ()
    _ -> return ()

--loadGame :: String -> InputT IO (Maybe String)
--loadGame s = do
--  g <- liftIO . try . readFile . last $ words s :: InputT IO (Either IOException String)
--  case g of
--    Right g' -> return $ Just g'
--    _        -> return Nothing

scoreIO :: Board -> InputT IO ()
scoreIO b = outputStrLn . (\(r,bl) -> "\n   " <> (if r == 0 then "\n" else show r <> "\n   ") <> concatMap cRed (replicate r ' ') <> "\n   " <> (if bl == 0 then "" else show bl) <> "\n   " <> concatMap cBlue (replicate bl ' ')) $ score b

screen :: Hexx -> InputT IO ()
screen (Hexx pt lm w oc b) = do
  liftIO resetCursor
  if oc == True then hexxagonTitle_c pt else hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge OnlyHexagons [] b
  return ()

screenWinner :: Hexx -> InputT IO ()
screenWinner (Hexx pt lm w oc b) = do
  liftIO resetCursor
  hexxagonWinner w
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge OnlyHexagons [] b
  return ()

testGI :: GameInfo
testGI = 
  GameInfo 
    [ RedPlayer  "6373" "tnr"
    , BluePlayer "6373" "tnb"
    ] 
    3600000 $ 
    Game 
      (BluePlayer "6373" "tnb")
      1811743228000 $ 
      rBoard $ read "[((3,4),'b'),((4,3),'b'),((4,4),'b'),((5,5),'r'),((5,6),'r'),((6,5),'r')]"
