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
  , checkGameStatus
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
import Data.Char 
  ( isDigit
  )
import Data.List
  ( inits
  )

playTurn :: GameInfo -> IO (Maybe RunGame)
playTurn gi@(GameInfo ps td (Game pt dl b)) = do
  epocht <- getPOSIXTime
  case checkGameStatus (round epocht * 1000) gi of
    Nothing -> do 
      hexx <- hexxagon (p2h pt) b
      case hexx of
        Just (Hexx _ _ lm w _ _) -> do
          epocht <- getPOSIXTime
          case w of
            Just Red   -> return . Just . GameOver $ h2p ps Red
            Just Blue  -> return . Just . GameOver $ h2p ps Blue
            Just Empty -> return $ Just Draw
            _ -> if   round epocht * 1000 > dl 
                 then return $ Just TimeOut 
                 else return . Just $ PlayTurn lm
        _ -> return Nothing
    _ -> return $ checkGameStatus (round epocht * 1000) gi

hexxagonHelp :: InputT IO ()
hexxagonHelp = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  outputStrLn ""
  outputStrLn "   Usage:"
  outputStrLn ""
  outputStrLn "     reset: Reset to initial state"
  outputStrLn "     finalize: Finalize first move"
  outputStrLn "     quit: Quit game"
  outputStrLn ""
  outputStrLn "     help: Print this help summary page"
  outputStrLn ""
  _ <- getInputLine "   "
  return ()

hexxagon :: Hexagon -> Board -> IO (Maybe Hexx)
hexxagon pt b = runInputT defaultSettings $ checkWinnerIO $ Hexx pt pt (Move (Position (-1) (-1)) (Position (-1) (-1))) Nothing b b

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
                             Just s' | any (== True) $ match s' <$> optionsList -> options s' $ Hexx opt pt lm w ob b'
                             _ -> checkWinnerIO $ Hexx opt pt lm (Just Red) ob b'
  Just (Blue, b')  -> if lm == fm then return . Just $ Hexx opt pt lm (Just Blue) ob b else
                        do screenWinner $ Hexx opt pt lm (Just Blue) ob b'; _ <- return . Just $ Hexx opt pt lm w ob b'
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO $ Hexx opt pt lm w ob b'
                             Just s' | any (== True) $ match s' <$> optionsList -> options s' $ Hexx opt pt lm w ob b'
                             _ -> checkWinnerIO $ Hexx opt pt lm (Just Blue) ob b'
  Just (Empty, b') -> if lm == fm then return . Just $ Hexx opt pt lm (Just Empty) ob b else
                        do screenWinner $ Hexx opt pt lm (Just Empty) ob b'; _ <- return . Just $ Hexx opt pt lm w ob b'
                           s <- getInputLine "   "
                           case s of
                             Just [] -> checkWinnerIO $ Hexx opt pt lm w ob b'
                             Just s' | any (== True) $ match s' <$> optionsList -> options s' $ Hexx opt pt lm w ob b'
                             _ -> checkWinnerIO $ Hexx opt pt lm (Just Blue) ob b'
  _ -> do screen g
          s <- getInputLine "   "
          case s of
            Just [] -> game g
            Just s' | any (== True) $ match s' <$> optionsList -> options s' g
            _ -> game g
  where
    optionsList = ["reset", "help", "quit", "finalize"]
    options s g'
      | match s "finalize"  = if lm == fm then checkWinnerIO g' else return $ Just g'
      | match s "help"   = do hexxagonHelp; checkWinnerIO g'
      | match s "reset"  = checkWinnerIO $ Hexx opt opt fm Nothing ob ob
      | otherwise = return Nothing

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
            0 -> checkWinnerIO $ Hexx opt opt lm w ob ob
            1 -> return Nothing 
            2 -> return $ Just g
      _ -> checkWinnerIO g
    Left g' -> case g' of
      0 -> checkWinnerIO $ Hexx opt opt lm w ob ob
      1 -> return Nothing
      2 -> return $ Just g

getFinalPosition :: Hexx -> Position -> InputT IO (Either Int (Maybe (Position, Integer)))
getFinalPosition g@(Hexx opt pt lm w ob b) ip = do
  liftIO resetCursor
  hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2])) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "help", "quit", "finalize"] -> posOptions s (flip getFinalPosition ip) g
    _ -> return . Right $ fp >>= parseInput >>= checkFinalPosition b . Move ip

getInitialPosition :: Hexx -> InputT IO (Either Int (Maybe Position))
getInitialPosition g@(Hexx opt pt lm w ob b@(Board mph)) = do
  liftIO resetCursor
  hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (filter (\p -> length (concat $ getNearbyPositions b p Empty <$> [1,2]) > 0) $ Map.keys $ Map.filter (== pt) mph)) b
  ip <- getInputLine "   "
  case ip of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "help", "quit", "finalize"] -> posOptions s getInitialPosition g
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

posOptions :: String -> (Hexx -> InputT IO (Either Int (Maybe b))) -> Hexx -> InputT IO (Either Int (Maybe b))
posOptions s f g@(Hexx opt pt lm w ob b)
  | match s "help"  = do hexxagonHelp; return . Right $ Nothing
  | match s "finalize" && lm /= fm = return $ Left 2
  | match s "quit" = return $ Left 1
  | otherwise  = return $ Left 0

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

createCustomBoard :: IO (Maybe Board)                                        
createCustomBoard = runInputT defaultSettings $ createCustomBoard' $ classicBoard_S9DC3

createCustomBoardHelp :: Board -> InputT IO (Maybe Board)
createCustomBoardHelp b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  outputStrLn ""
  outputStrLn "   Usage:"
  outputStrLn ""
  outputStrLn "     size <3,5,7,9>: Board size"
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
  outputStrLn "     quit: Quit board configuration"
  outputStrLn ""
  outputStrLn "     help: Print this help summary page"
  outputStrLn ""
  _ <- getInputLine "   "
  createCustomBoard' b

createCustomBoard' :: Board -> InputT IO (Maybe Board)
createCustomBoard' b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords $ fst <$> Map.toList (boardToMap b)) b
  input <- getInputLine "   "
  case input of
    Just [] -> createCustomBoard' b
    Just s
      | match s "blue"     -> createCustomBoard' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Blue)) <$> (parseConfig . tail $ words s)
      | match s "delete"   -> if match (last $ words s) "all" then createCustomBoard' . Board $ Map.fromList [] else createCustomBoard' . modifyBoard b $ (\(x,y) -> Left (Position x y)) <$> (parseConfig . tail $ words s)
      | match s "empty"    -> createCustomBoard' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Empty)) <$> (parseConfig . tail $ words s)
      | match s "red"      -> createCustomBoard' . modifyBoard b $ (\(x,y) -> Right ((Position x y), Red)) <$> (parseConfig . tail $ words s)
      | match s "all"      -> allM (last $ words s)
      | match s "help"     -> createCustomBoardHelp b
      | match s "size" && if isDigit . last . last $ words s then (elem (sParse s) [3,5,7,9]) else False -> createCustomBoard' $ makeStartingBoard (read . last $ words s :: Integer) []
      | elem s $ drop 3 $ inits "reset" -> createCustomBoard' classicBoard_S9DC3
      | s ==    "finalize" -> return $ Just b 
      | match s "quit"     -> return Nothing
    _ -> createCustomBoard' b
    where
      sParse s = read . last $ words s :: Integer
      allM s
        | match s "blue"   = createCustomBoard' . Board $ foldr (\x -> Map.insert x Blue) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
        | match s "empty"  = createCustomBoard' . Board $ foldr (\x -> Map.insert x Empty) (boardToMap b) $ fst <$> Map.toList (boardToMap b )
        | match s "red"    = createCustomBoard' . Board $ foldr (\x -> Map.insert x Red) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
        | otherwise = createCustomBoard' b
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
      1911743228000 $ 
      rBoard $ read "[((1,1),'b'),((1,2),'b'),((1,3),'b'),((1,4),'b')]"
