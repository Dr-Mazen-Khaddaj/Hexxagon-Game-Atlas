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
import Instances
  ( destructureBoard
  )

playTurn :: GameInfo -> IO RunGame
playTurn (GameInfo ps td (Game pt dl b)) = do
  posixtime <- getPOSIXTime
  if   round posixtime * 1000 > dl 
  then return TimeOut 
  else do
    hexx <- hexxagon (p2h pt) b
    case hexx of
      Just (Hexx opt pt lm w ob b) -> do
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

hexxagonHelp :: InputT IO ()
hexxagonHelp = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  outputStrLn ""
  outputStrLn "   Usage:"
  outputStrLn ""
  outputStrLn "     reset: Reset to initial state"
  outputStrLn "     finalize: Finalize first move"
  outputStrLn ""
  outputStrLn "     help: Print this help summary page"
  outputStrLn ""
  _ <- getInputLine "   "
  return ()

hexxagon :: Hexagon -> Board -> IO (Maybe Hexx)
hexxagon pt b = do 
  runInputT defaultSettings $ hexxagonHelp
  runInputT defaultSettings $ checkWinnerIO $ Hexx pt pt (Move (Position (-1) (-1)) (Position (-1) (-1))) Nothing b b

cBlue :: Char -> [Char]
cBlue c = setSGRCode [SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]

cRed :: Char -> [Char]
cRed  c = setSGRCode [SetColor Background Dull C.Red]  <> [c] <> setSGRCode [Reset]

fm = Move (Position (-1) (-1)) (Position (-1) (-1))

checkWinnerIO :: Hexx -> InputT IO (Maybe Hexx)
checkWinnerIO g@(Hexx opt pt lm w ob b) = case checkWinner b of
  Just (Red, b')   -> if lm == fm then return . Just $ Hexx opt pt lm (Just Red) ob b else
                        do screenWinner $ Hexx opt pt lm (Just Red) ob b'
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO $ Hexx opt pt lm w ob b'
                             Just s' | any (== True) $ match s' <$> ["reset", "finalize", "help"] -> resetFinal s' $ Hexx opt pt lm w ob b'
                             _ -> checkWinnerIO $ Hexx opt pt lm (Just Red) ob b'
  Just (Blue, b')  -> if lm == fm then return . Just $ Hexx opt pt lm (Just Blue) ob b else
                        do screenWinner $ Hexx opt pt lm (Just Blue) ob b'; _ <- return . Just $ Hexx opt pt lm w ob b'
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO $ Hexx opt pt lm w ob b'
                             Just s' | any (== True) $ match s' <$> ["reset", "finalize", "help"] -> resetFinal s' $ Hexx opt pt lm w ob b'
                             _ -> checkWinnerIO $ Hexx opt pt lm (Just Blue) ob b'
  Just (Empty, b') -> if lm == fm then return . Just $ Hexx opt pt lm (Just Empty) ob b else
                        do screenWinner $ Hexx opt pt lm (Just Empty) ob b'; _ <- return . Just $ Hexx opt pt lm w ob b'
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO $ Hexx opt pt lm w ob b'
                             Just s' | any (== True) $ match s' <$> ["reset", "finalize", "help"] -> resetFinal s' $ Hexx opt pt lm w ob b'
                             _ -> checkWinnerIO $ Hexx opt pt lm (Just Blue) ob b'
  _ -> do screen g
          s <- getInputLine "   "
          case s of
            Just [] -> game g
            Just s' | any (== True) $ match s' <$> ["reset", "finalize", "help"] -> resetFinal s' g
            _ -> game g
  where
    resetFinal s g_@(Hexx opt pt lm w oc _)
      | match s "help"   = do hexxagonHelp; checkWinnerIO g_
      | match s "reset"  = checkWinnerIO $ Hexx opt opt fm Nothing ob ob
      | otherwise        = if lm == fm && w == Nothing then checkWinnerIO g_ else return . Just $ Hexx opt pt lm w ob b

colorize :: Char -> [Char]
colorize c
  | 'R' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Dull C.Red] <> [c] <> setSGRCode [Reset]
  | 'B' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]
  | '-' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Black] <> "E" <> setSGRCode [Reset]
  | otherwise = [c]

game :: Hexx -> InputT IO (Maybe Hexx)
game g@(Hexx opt pt lm w ob b) = do
  ip <- getInitialPosition g
  case ip of
    Right ip' -> case ip' of
      Just ip'' -> do
        fp <- getFinalPosition g ip''
        case fp of
          Right fp' -> case fp' of
            Just (fp'', d) -> do
              case makeMove (Hexx opt pt lm w ob b) (Move ip'' fp'') d of
                Just (Hexx opt' pt' lm' w' ob' b') -> if lm == fm then checkWinnerIO (Hexx opt' pt' (Move ip'' fp'') w' ob' b') else checkWinnerIO (Hexx opt' pt' lm' w' ob' b')
                _ -> return Nothing
            _ -> checkWinnerIO g
          Left g' -> case g' of
            Just g'' -> return $ Just g''
            _ -> checkWinnerIO $ Hexx opt opt lm w ob ob
      _ -> checkWinnerIO g
    Left g' -> case g' of
      Just g'' -> return $ Just g''
      _ -> checkWinnerIO $ Hexx opt opt lm w ob ob

getFinalPosition :: Hexx -> Position -> InputT IO (Either (Maybe Hexx) (Maybe (Position, Integer)))
getFinalPosition g@(Hexx opt pt lm w ob b) ip = do
  liftIO resetCursor
  hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2])) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "finalize", "help"] -> posresetFinal s (flip getFinalPosition ip) g
    _ -> return . Right $ fp >>= parseInput >>= checkFinalPosition b . Move ip

getInitialPosition :: Hexx -> InputT IO (Either (Maybe Hexx) (Maybe Position))
getInitialPosition g@(Hexx opt pt lm w ob b@(Board mph)) = do
  liftIO resetCursor
  hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (filter (\p -> length (concat $ getNearbyPositions b p Empty <$> [1,2]) > 0) $ Map.keys $ Map.filter (== pt) mph)) b
  ip <- getInputLine "   "
  case ip of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "finalize", "help"] -> posresetFinal s getInitialPosition g
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

posresetFinal :: String -> (Hexx -> InputT IO (Either (Maybe Hexx) (Maybe b))) -> Hexx -> InputT IO (Either (Maybe Hexx) (Maybe b))
posresetFinal s f g@(Hexx opt pt lm w ob b)
  | match s "help"  = do hexxagonHelp; return . Right $ Nothing
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
screen (Hexx opt pt lm w ob b) = do
  liftIO resetCursor
  hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords []) b
  return ()

screenWinner :: Hexx -> InputT IO ()
screenWinner (Hexx opt pt lm w ob b) = do
  liftIO resetCursor
  hexxagonWinner w
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords []) b
  return ()

createCustomBoard :: IO Board                                        
createCustomBoard = runInputT defaultSettings $ createCustomBoard'

createCustomBoard' :: InputT IO Board
createCustomBoard' = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  outputStrLn ""
  outputStrLn "   Usage:"
  outputStrLn ""
  outputStrLn "     red <space delimited coordinates>: Make selected hexagons red"
  outputStrLn "     blue <space delimited coordinates>: Make selected hexagons blue"
  outputStrLn "     empty <space delimited coordinates>: Make selected hexagons empty"
  outputStrLn "     delete <space delimited coordinates>: Delete selected hexagons"
  outputStrLn ""
  outputStrLn "     all red: Make all hexagons red"
  outputStrLn "     all blue: Make all hexagons blue"
  outputStrLn "     all empty: Make all hexagons empty"
  outputStrLn "     delete all: Delete all hexagons"
  outputStrLn ""
  outputStrLn "     reset: Reset to default board"
  outputStrLn "     finalize: Finalize board"
  outputStrLn ""
  outputStrLn "     help: Print this help summary page"
  outputStrLn ""
  _ <- getInputLine "   "
  createCustomBoard'' classicBoard_S9DC3

createCustomBoard'' :: Board -> InputT IO Board
createCustomBoard'' b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords $ fst <$> Map.toList (boardToMap b)) b
  input <- getInputLine "   "
  case input of
    Just [] -> createCustomBoard'' b
    Just s
      | match s "blue"     -> createCustomBoard'' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Blue)) <$> (parseConfig . tail $ words s)
      | match s "delete"   -> if match (last $ words s) "all" then createCustomBoard'' . Board $ Map.fromList [] else createCustomBoard'' . modifyBoard b $ (\(x,y) -> Left (Position x y)) <$> (parseConfig . tail $ words s)
      | match s "empty"    -> createCustomBoard'' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Empty)) <$> (parseConfig . tail $ words s)
      | match s "red"      -> createCustomBoard'' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Red)) <$> (parseConfig . tail $ words s)
      | match s "all"      -> allM (last $ words s)
      | match s "help"     -> createCustomBoard'
      | s ==    "reset"    -> createCustomBoard'' classicBoard_S9DC3
      | s ==    "finalize" -> return b 
    _ -> createCustomBoard'' b
    where
      allM s
        | match s "blue"   = createCustomBoard'' . Board $ foldr (\x -> Map.insert x Blue) (boardToMap classicBoard_S9DC3) $ fst <$> Map.toList (boardToMap classicBoard_S9DC3)
        | match s "empty"  = createCustomBoard'' . Board $ foldr (\x -> Map.insert x Empty) (boardToMap classicBoard_S9DC3) $ fst <$> Map.toList (boardToMap classicBoard_S9DC3)
        | match s "red"    = createCustomBoard'' . Board $ foldr (\x -> Map.insert x Red) (boardToMap classicBoard_S9DC3) $ fst <$> Map.toList (boardToMap classicBoard_S9DC3)
        | otherwise = createCustomBoard'' b
      boardToMap (Board b) = b

testGI :: GameInfo
testGI = 
  GameInfo 
    [ RedPlayer  "6373" "tnr"
    , BluePlayer "6373" "tnb"
    ] 
    3600000 $ 
    Game 
      (BluePlayer "6373" "tnb")
      1211743228000 $ 
      rBoard $ read "[((5,6),'r'),((5,7),'e'),((5,8),'b'),((6,6),'b'),((6,7),'b'),((6,8),'b')]"
