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
  , destructureGame
  , restructureGame
  , boardToMap
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
  )
import PlutusTx.AssocMap qualified as Map
import qualified System.Console.ANSI as C (Color(..))
import Constants
  ( classicBoard_S9DC3
  )
import Data.Time.Clock.POSIX 
  ( getPOSIXTime
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

hexxHGame :: Hexx -> InputT IO (Maybe Hexx)
hexxHGame g = do
  hexxagonHelp
  s <- getInputLine "   "
  hexxGameOptions s g

posHexxHGame :: (Hexx -> InputT IO (Either Int b)) -> Hexx -> InputT IO (Either Int b)
posHexxHGame f g = do
  hexxagonHelp
  s <- getInputLine "   "
  case s of
    Just s' -> posHexxGameOptions s' f g
    _       -> posHexxHGame f g

hexxGameOptions :: Maybe String -> Hexx -> InputT IO (Maybe Hexx)
hexxGameOptions st ga = do
  case st of
    Just [] -> checkWinnerIO ga
    Just s  -> lsqh s ga
    _       -> checkWinnerIO ga
    where
      lsqh s g@(Hexx opt _ lm _ ob _)
        | match s "help"  = hexxHGame g
        | match s "quit"  = return Nothing
        | match s "reset" = checkWinnerIO $ Hexx opt opt fm Nothing ob ob
        | match s "finalize" = if lm == fm then checkWinnerIO g else return $ Just g
        | otherwise = checkWinnerIO g

posHexxGameOptions :: String -> (Hexx -> InputT IO (Either Int b)) -> Hexx -> InputT IO (Either Int b)
posHexxGameOptions s f g@(Hexx _ _ lm _ _ _)
  | s == [] = f g
  | match s "help" = posHexxHGame f g
  | match s "finalize" = if lm == fm then return $ Left 2 else return $ Left 3
  | match s "quit" = return $ Left 1
  | otherwise = return $ Left 0

hexxagonHelp :: InputT IO ()
hexxagonHelp = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  outputStrLn ""
  outputStrLn "   Enter coordinates to choose which piece to move and where."
  outputStrLn "   Pieces can move one or two spaces in either direction."
  outputStrLn "   Moving a peice one space will clone it, two will hop."
  outputStrLn "   Adjacent pieces that are your opponent will convert."
  outputStrLn "   Player with the most pieces wins."
  outputStrLn ""
  outputStrLn "     reset: Restart to current game state"
  outputStrLn "     finalize: Submit first move to blockchain"
  outputStrLn "     quit: Exit game"
  outputStrLn "     help: Print this help summary page."
  outputStrLn ""
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
  Just (Red, b')   -> do screenWinner $ Hexx opt pt lm (Just Red  ) ob b'
                         s <- getInputLine "   "
                         hexxGameOptions s g
  Just (Blue, b')  -> do screenWinner $ Hexx opt pt lm (Just Blue ) ob b'
                         s <- getInputLine "   "
                         hexxGameOptions s g
  Just (Empty, b') -> do screenWinner $ Hexx opt pt lm (Just Empty) ob b'
                         s <- getInputLine "   "
                         hexxGameOptions s g
  _ -> do screen g
          s <- getInputLine "   "
          case s of
            Just [] -> game g
            _ -> hexxGameOptions s g

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
            2 -> checkWinnerIO g 
            3 -> return $ Just g
      _ -> checkWinnerIO g
    Left g' -> case g' of
      0 -> checkWinnerIO $ Hexx opt opt lm w ob ob
      1 -> return Nothing
      2 -> checkWinnerIO g
      3 -> return $ Just g

getFinalPosition :: Hexx -> Position -> InputT IO (Either Int (Maybe (Position, Integer)))
getFinalPosition g@(Hexx opt pt lm w ob b) ip = do
  liftIO resetCursor
  hexxagonTitle pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2])) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "help", "quit", "finalize"] -> posHexxGameOptions s (flip getFinalPosition ip) g
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
    Just s | any (== True) $ match s <$> ["reset", "help", "quit", "finalize"] -> posHexxGameOptions s getInitialPosition g
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
createCustomBoard = runInputT defaultSettings $ createCustomBoard' classicBoard_S9DC3

createCustomBoardHelp :: Board -> InputT IO (Maybe Board)
createCustomBoardHelp b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  outputStrLn ""
  outputStrLn "     Enter 'play' or create a custom board first."
  outputStrLn ""
  outputStrLn "     size <1,2,3,4>: Adjust board size"
  outputStrLn "" 
  outputStrLn "     red <space delimited coordinates>: Make selected hexagons red"
  outputStrLn "     blue <space delimited coordinates>: Make selected hexagons blue"
  outputStrLn "     empty <space delimited coordinates>: Make selected hexagons empty"
  outputStrLn "     delete <space delimited coordinates>: Remove selected hexagons"
  outputStrLn ""
  outputStrLn "     all red: Make all hexagons red"
  outputStrLn "     all blue: Make all hexagons blue"
  outputStrLn "     all empty: Make all hexagons empty"
  outputStrLn "     delete all: Remove all hexagons"
  outputStrLn ""
  outputStrLn "     play: Start Game"
  outputStrLn "     save <filepath>: Save board layout"
  outputStrLn "     load <filepath>: Load board layout"
  outputStrLn "     reset: Default board"
  outputStrLn "     quit: Exit Game"
  outputStrLn "     help: Print this help summary page."
  outputStrLn ""
  input <- getInputLine "   "
  createCustomBoardOptions createCustomBoard' b input

createCustomBoard' :: Board -> InputT IO (Maybe Board)
createCustomBoard' b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  scoreIO b                
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords $ fst <$> Map.toList (boardToMap b)) b
  input <- getInputLine "   "
  createCustomBoardOptions createCustomBoard' b input

createCustomBoardOptions :: (Board -> InputT IO (Maybe Board)) -> Board -> Maybe String -> InputT IO (Maybe Board)
createCustomBoardOptions func b input = case input of 
  Just [] -> func b
  Just s
    | match s "blue"     -> func . modifyBoard b $ (\(x,y) -> Right ((Position x y), Blue)) <$> (parseConfig . tail $ words s)
    | match s "delete"   -> if match (last $ words s) "all" then func . Board $ Map.fromList [] else func . modifyBoard b $ (\(x,y) -> Left (Position x y)) <$> (parseConfig . tail $ words s)
    | match s "empty"    -> func . modifyBoard b $ (\(x,y) -> Right ((Position x y), Empty)) <$> (parseConfig . tail $ words s)
    | match s "red"      -> func . modifyBoard b $ (\(x,y) -> Right ((Position x y), Red)) <$> (parseConfig . tail $ words s)
    | match s "all"      -> allM (last $ words s)
    | match s "help"     -> createCustomBoardHelp b
    | match s "size" && if isDigit . last . last $ words s then (elem (sParse s) [1,2,3,4]) else False -> func $ makeStartingBoard (sParseNum (read . last $ words s :: Integer)) []
    | match s "play"     -> return $ Just b
    | match s "quit"     -> return Nothing
    | match s "load"     -> do g' <- loadGame s
                               case g' of
                                 Just g'' -> do
                                   let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                   func . snd $ restructureGame g'''
                                 _ -> func b
    | match s "save"     -> do saveGame (Red, b) s; func b
    | elem s $ drop 3 $ inits "reset" -> createCustomBoard' classicBoard_S9DC3
  _ -> func b
  where                
    sParse s = read . last $ words s :: Integer
    sParseNum s = case s of
      1 -> 3
      2 -> 5
      3 -> 7
      _ -> 9
    allM s                             
      | match s "blue"   = func . Board $ foldr (\x -> Map.insert x Blue) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
      | match s "empty"  = func . Board $ foldr (\x -> Map.insert x Empty) (boardToMap b) $ fst <$> Map.toList (boardToMap b )
      | match s "red"    = func . Board $ foldr (\x -> Map.insert x Red) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
      | otherwise = func b

saveGame :: (Hexagon, Board) -> String -> InputT IO ()
saveGame g s = do
  g' <- liftIO . try . writeFile (last $ words s) . show $ destructureGame g :: InputT IO (Either IOException ())
  case g' of
    Right _ -> return ()
    _ -> return ()

loadGame :: String -> InputT IO (Maybe String)
loadGame s = do                                                                    
  g <- liftIO . try . readFile . last $ words s :: InputT IO (Either IOException String)
  case g of                                
    Right g' -> return $ Just g'
    _        -> return Nothing

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
      rBoard $ read "[((1,1),'r'),((1,2),'e'),((1,3),'b'),((2,1),'e'),((2,2),'e'),((2,3),'e'),((2,4),'e'),((3,1),'b'),((3,2),'e'),((3,3),'e'),((3,4),'e'),((3,5),'r'),((4,2),'e'),((4,3),'e'),((4,4),'e'),((4,5),'e'),((5,3),'r'),((5,4),'e'),((5,5),'b')]"
