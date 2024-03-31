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
  , makeMove
  , match
  , modifyBoard
  , parseConfig
  , parseInput
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
hexxagon pt b = runInputT defaultSettings $ checkWinnerIO 0 $ Hexx pt (Move (Position (-1) (-1)) (Position (-1) (-1))) Nothing b b

cBlue :: Char -> [Char]
cBlue c = setSGRCode [SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]

cRed :: Char -> [Char]
cRed  c = setSGRCode [SetColor Background Dull C.Red]  <> [c] <> setSGRCode [Reset]

fm = Move (Position (-1) (-1)) (Position (-1) (-1))

checkWinnerIO :: Integer -> Hexx -> InputT IO (Maybe Hexx)
checkWinnerIO c g@(Hexx pt lm w oc b) = case checkWinner b of
  Just (Red, b')   -> if lm == fm then return . Just $ Hexx pt lm (Just Red) oc b else
                        do screenWinner $ Hexx pt lm (Just Red) oc b'
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO c $ Hexx pt lm (Just Red) oc b'
                             Just s' | any (== True) $ match s' <$> ["reset", "finalize"] -> resetFinal s' $ Hexx pt lm (Just Red) oc b'
                             _ -> checkWinnerIO c $ Hexx pt lm (Just Red) oc b'
  Just (Blue, b')  -> if lm == fm then return . Just $ Hexx pt lm (Just Blue) oc b else
                        do screenWinner $ Hexx pt lm (Just Blue) oc b'; _ <- return . Just $ Hexx pt lm w oc b'
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO c $ Hexx pt lm (Just Blue) oc b'
                             Just s' | any (== True) $ match s' <$> ["reset", "finalize"] -> resetFinal s' $ Hexx pt lm (Just Blue) oc b'
                             _ -> checkWinnerIO c $ Hexx pt lm (Just Blue) oc b'
  Just (Empty, b') -> if lm == fm then return . Just $ Hexx pt lm (Just Empty) oc b else
                        do screenWinner $ Hexx pt lm (Just Empty) oc b'; _ <- return . Just $ Hexx pt lm w oc b'
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO c $ Hexx pt lm (Just Empty) oc b'
                             Just s' | any (== True) $ match s' <$> ["reset", "finalize"] -> resetFinal s' $ Hexx pt lm (Just Empty) oc b'
                             _ -> checkWinnerIO c $ Hexx pt lm (Just Blue) oc b'
  _ -> do screen g
          s <- getInputLine "   "
          case s of
            Just [] -> game (c+1) g
            Just s' | any (== True) $ match s' <$> ["reset", "finalize"] -> resetFinal s' g
            _ -> game (c+1) g
  where
    resetFinal s g_@(Hexx _ _ w oc _)
      | match s "reset"  = checkWinnerIO 0 $ Hexx (succ pt) fm Nothing oc oc
      | otherwise        = if lm == fm && w == Nothing then checkWinnerIO c g_ else return . Just $ Hexx pt lm w oc b

colorize :: Char -> [Char]
colorize c
  | 'R' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Dull C.Red] <> [c] <> setSGRCode [Reset]
  | 'B' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]
  | 'E' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Black] <> [c] <> setSGRCode [Reset]
  | otherwise = [c]

game :: Integer -> Hexx -> InputT IO (Maybe Hexx)
game c g@(Hexx pt lm w oc b) = do
  ip <- getInitialPosition g
  case ip of
    Right ip' -> case ip' of
      Just ip'' -> do
        fp <- getFinalPosition g ip''
        case fp of
          Right fp' -> case fp' of
            Just (fp'', d) -> do
              case makeMove (Hexx pt lm w oc b) (Move ip'' fp'') d of
                Just (Hexx pt' lm' w' oc' b') -> if c == 1 then checkWinnerIO c (Hexx pt' (Move ip'' fp'') w' oc' b') else checkWinnerIO c (Hexx pt' lm' w' oc' b')
                _ -> return Nothing
            _ -> checkWinnerIO (if lm == fm then 0 else c) g
          Left g' -> case g' of
            Just g'' -> checkWinnerIO (if lm == fm then 0 else c) g''
            _ -> return g' 
      _ -> checkWinnerIO c g
    Left g' -> case g' of
      Just g'' -> checkWinnerIO (if lm == fm then 0 else c) g''
      _ -> return g'

getFinalPosition :: Hexx -> Position -> InputT IO (Either (Maybe Hexx) (Maybe (Position, Integer)))
getFinalPosition g@(Hexx pt lm w oc b) ip = do
  liftIO resetCursor
  hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2]) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "finalize"] -> posresetFinal s (flip getFinalPosition ip) g
    _ -> return . Right $ fp >>= parseInput >>= checkFinalPosition b . Move ip

getInitialPosition :: Hexx -> InputT IO (Either (Maybe Hexx) (Maybe Position))
getInitialPosition g@(Hexx pt lm w oc b@(Board mph)) = do
  liftIO resetCursor
  hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge SelectiveCoords (filter (\p -> length (concat $ getNearbyPositions b p Empty <$> [1,2]) > 0) $ Map.keys $ Map.filter (== pt) mph) b
  ip <- getInputLine "   "
  case ip of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "finalize"] -> posresetFinal s getInitialPosition g
    _ -> return . Right $ ip >>= parseInput >>= checkInitialPosition g

hexxagonTitle :: Hexagon -> InputT IO ()
hexxagonTitle h = 
  if   h == Red 
  then outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull C.Red  , SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  else outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]

hexxagonWinner :: Maybe Hexagon -> InputT IO ()
hexxagonWinner w = case w of
  Just Red  -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull  C.Red ,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  Just Blue -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  _         -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN TIE! " <> setSGRCode [Reset]

posresetFinal :: String -> (Hexx -> InputT IO (Either (Maybe Hexx) b)) -> Hexx -> InputT IO (Either (Maybe Hexx) b)
posresetFinal s f g@(Hexx _ _ _ oc _)
  | match s "reset"  = return . Left $ Nothing
  | otherwise        = return . Left $ Just g

resetCursor :: IO ()
resetCursor = do
  clearScreen
  setCursorPosition 0 0
  hFlush stdout

scoreIO :: Board -> InputT IO ()
scoreIO b = outputStrLn . (\(r,bl) -> "\n   " <> (if r == 0 then "\n" else show r <> "\n   ") <> concatMap cRed (replicate r ' ') <> "\n   " <> (if bl == 0 then "" else show bl) <> "\n   " <> concatMap cBlue (replicate bl ' ')) $ score b

screen :: Hexx -> InputT IO ()
screen (Hexx pt lm w oc b) = do
  liftIO resetCursor
  hexxagonTitle pt
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
      1911743228000 $ 
      rBoard $ read "[((5,6),'b'),((5,7),'b'),((5,8),'b'),((6,6),'b'),((6,7),'b'),((6,8),'b')]"
