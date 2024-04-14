module IOFxs where

import Constants
  ( classicBoard_S9DC3
  )
import Control.Exception                                                      
  ( IOException
  , try                
  )    
import Control.Monad.State
import Data.Char 
  ( isDigit
  )
import Data.List
  ( inits
  )
import Data.Time.Clock.POSIX 
  ( getPOSIXTime
  )
import DataTypes 
  ( Board(..)
  , GameInfo(..)
  , GameState(..)
  , Hexagon(..)
  , Hexx(..)
  , Move(..)
  , Player(..)
  , Position(..)
  , RunGame(..)
  )
import Instances 
  ( BoardMode(..)
  , Orientation(Edge)
  , showBoard
  )
import MainFxs 
  ( boardToMap
  , checkFinalPosition
  , checkGameStatus
  , checkInitialPosition
  , checkWinner
  , destructureGame
  , hexagonToPlayer
  , listToBoard
  , makeMove
  , match
  , modifyBoard
  , parseConfig
  , parseInput
  , playerToHexagon
  , restructureGame
  , score
  )
import PlutusTx.AssocMap qualified as Map
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
import qualified System.Console.ANSI as C (Color(..))

boardConfig :: Board -> InputT IO (Maybe Board)
boardConfig b = do
  liftIO resetCursor
  outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN BOARD CONFIGURATION " <> setSGRCode [Reset]
  scoreIO b                
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords $ fst <$> Map.toList (boardToMap b)) b
  boardConfigCommands b

boardConfigCommands :: Board -> InputT IO (Maybe Board)
boardConfigCommands b = do
  input <- getInputLine "   "
  case input of
    Just [] -> boardConfig b
    Just s
      | match s "blue"   -> boardConfig . modifyBoard b $ (\(x,y) -> Right ((Position x y), Blue)) <$> (parseConfig . tail $ words s)
      | match s "delete" -> if match (last $ words s) "all" then boardConfig . Board $ Map.fromList [] else boardConfig . modifyBoard b $ (\(x,y) -> Left (Position x y)) <$> (parseConfig . tail $ words s)
      | match s "empty"  -> boardConfig . modifyBoard b $ (\(x,y) -> Right ((Position x y), Empty)) <$> (parseConfig . tail $ words s)
      | match s "red"    -> boardConfig . modifyBoard b $ (\(x,y) -> Right ((Position x y), Red)) <$> (parseConfig . tail $ words s)
      | match s "all"    -> allM (last $ words s)
      | match s "help"   -> boardConfigHelp b
      | match s "size" && if isDigit . last . last $ words s then (elem (sParse s) [1,2,3,4]) else False -> boardConfig $ makeStartingBoard (sParseNum (read . last $ words s :: Integer)) []
      | match s "play"   -> return $ Just b
      | match s "quit"   -> return Nothing
      | match s "load"   -> do g' <- loadGame s
                               case g' of
                                 Just g'' -> do
                                   let g''' = read g'' :: (Char, [((Integer, Integer), Char)])
                                   boardConfig . snd $ restructureGame g'''
                                 _ -> boardConfig b
      | match s "save"     -> do saveGame (Red, b) s; boardConfig b
      | elem s $ drop 3 $ inits "reset" -> boardConfig classicBoard_S9DC3
    _ -> boardConfig b
    where                
      sParse s = read . last $ words s :: Integer
      sParseNum s = case s of
        1 -> 3
        2 -> 5
        3 -> 7
        _ -> 9
      allM s                             
        | match s "blue"   = boardConfig . Board $ foldr (\x -> Map.insert x Blue) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
        | match s "empty"  = boardConfig . Board $ foldr (\x -> Map.insert x Empty) (boardToMap b) $ fst <$> Map.toList (boardToMap b )
        | match s "red"    = boardConfig . Board $ foldr (\x -> Map.insert x Red) (boardToMap b) $ fst <$> Map.toList (boardToMap b)
        | otherwise = boardConfig b

boardConfigHelp :: Board -> InputT IO (Maybe Board)
boardConfigHelp b = do
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
  boardConfigCommands b

boardConfigRunInputT :: IO (Maybe Board)                                        
boardConfigRunInputT = runInputT defaultSettings $ boardConfig classicBoard_S9DC3

checkWinnerIO :: Hexx -> InputT IO (Maybe Hexx)
checkWinnerIO g@(Hexx b fb fm fpt pt _) = case checkWinner b of
  Just (Red, b')   -> do screenWinner $ Hexx b' fb fm fpt pt $ Just Red
                         s <- getInputLine "   "
                         gameCommands s g
  Just (Blue, b')  -> do screenWinner $ Hexx b' fb fm fpt pt $ Just Blue
                         s <- getInputLine "   "
                         gameCommands s g
  Just (Empty, b') -> do screenWinner $ Hexx b' fb fm fpt pt $ Just Empty
                         s <- getInputLine "   "
                         gameCommands s g
  _ -> do screenDefault g
          s <- getInputLine "   "
          case s of
            Just [] -> getPositions g
            _ -> gameCommands s g

colorBlue :: Char -> [Char]
colorBlue c = setSGRCode [SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]

colorRed :: Char -> [Char]
colorRed  c = setSGRCode [SetColor Background Dull C.Red]  <> [c] <> setSGRCode [Reset]

colorize :: Char -> [Char]
colorize c
  | 'R' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Dull C.Red] <> [c] <> setSGRCode [Reset]
  | 'B' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Blue] <> [c] <> setSGRCode [Reset]
  | '-' == c = setSGRCode [SetColor Foreground Dull C.Black, SetColor Background Vivid C.Black] <> "E" <> setSGRCode [Reset]
  | otherwise = [c]

createCustomBoard :: IO (Maybe Board)                                        
createCustomBoard = boardConfigRunInputT

defaultMove :: Move
defaultMove = Move (Position (-1) (-1)) (Position (-1) (-1))

gameCommands :: Maybe String -> Hexx -> InputT IO (Maybe Hexx)
gameCommands s g = do
  case s of
    Just [] -> checkWinnerIO g
    Just s' -> lsqh s' g
    _       -> checkWinnerIO g
    where
      lsqh s' g'@(Hexx _ fb fm fpt _ _)
        | match s' "help"  = gameHelp g'
        | match s' "quit"  = return Nothing
        | match s' "reset" = checkWinnerIO $ Hexx fb fb defaultMove fpt fpt Nothing
        | match s' "finalize" = if fm == defaultMove then checkWinnerIO g' else return $ Just g'
        | otherwise = checkWinnerIO g

gameHelp :: Hexx -> InputT IO (Maybe Hexx)
gameHelp g = do
  screenGameHelp
  s <- getInputLine "   "
  gameCommands s g

getFinalPosition :: Hexx -> Position -> InputT IO (Either Int (Maybe (Position, Integer)))
getFinalPosition g@(Hexx b _ _ _ pt _) ip = do
  liftIO resetCursor
  titleDefault pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (concatMap (getNearbyPositions b ip Empty) [1,2])) b
  fp <- getInputLine "   "
  case fp of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "help", "quit", "finalize"] -> getPosGameCommands s (flip getFinalPosition ip) g
    _ -> return . Right $ fp >>= parseInput >>= checkFinalPosition b . Move ip

getInitialPosition :: Hexx -> InputT IO (Either Int (Maybe Position))
getInitialPosition g@(Hexx b@(Board mph) _ _ _ pt _) = do
  liftIO resetCursor
  titleDefault pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords (filter (\p -> length (concat $ getNearbyPositions b p Empty <$> [1,2]) > 0) $ Map.keys $ Map.filter (== pt) mph)) b
  ip <- getInputLine "   "
  case ip of
    Just [] -> return $ Right Nothing
    Just s | any (== True) $ match s <$> ["reset", "help", "quit", "finalize"] -> getPosGameCommands s getInitialPosition g
    _ -> return . Right $ ip >>= parseInput >>= checkInitialPosition g

getPosGameCommands :: String -> (Hexx -> InputT IO (Either Int b)) -> Hexx -> InputT IO (Either Int b)
getPosGameCommands s f g@(Hexx _ _ fm _ _ _)
  | s == [] = f g
  | match s "help" = getPosGameHelp f g
  | match s "finalize" = if fm == defaultMove then return $ Left 2 else return $ Left 3
  | match s "quit" = return $ Left 1
  | otherwise = return $ Left 0

getPosGameHelp :: (Hexx -> InputT IO (Either Int b)) -> Hexx -> InputT IO (Either Int b)
getPosGameHelp f g = do
  screenGameHelp
  s <- getInputLine "   "
  case s of
    Just s' -> getPosGameCommands s' f g
    _       -> getPosGameHelp f g

getPositions :: Hexx -> InputT IO (Maybe Hexx)
getPositions g@(Hexx b fb fm fpt pt w) = do
  ip <- getInitialPosition g
  case ip of
    Right ip' -> case ip' of
      Just ip'' -> do
        fp <- getFinalPosition g ip''
        case fp of
          Right fp' -> case fp' of
            Just (fp'', d) -> do
              case makeMove (Hexx b fb fm fpt pt w) (Move ip'' fp'') d of
                Just (Hexx b' fb' fm' fpt' pt' w') -> if fm == defaultMove then checkWinnerIO (Hexx b' fb' (Move ip'' fp'') fpt' pt' w') else checkWinnerIO (Hexx b' fb' fm' fpt' pt' w')
                _ -> return Nothing
            _ -> checkWinnerIO g
          Left g' -> case g' of
            0 -> checkWinnerIO $ Hexx fb fb fm fpt fpt w
            1 -> return Nothing 
            2 -> checkWinnerIO g 
            _ -> return $ Just g
      _ -> checkWinnerIO g
    Left g' -> case g' of
      0 -> checkWinnerIO $ Hexx fb fb fm fpt fpt w
      1 -> return Nothing
      2 -> checkWinnerIO g
      _ -> return $ Just g

hexxagonRunInputT :: Hexagon -> Board -> IO (Maybe Hexx)
hexxagonRunInputT pt b = runInputT defaultSettings $ checkWinnerIO $ Hexx b b (Move (Position (-1) (-1)) (Position (-1) (-1))) pt pt Nothing

loadGame :: String -> InputT IO (Maybe String)
loadGame s = do                                                                    
  g <- liftIO . try . readFile . last $ words s :: InputT IO (Either IOException String)
  case g of                                
    Right g' -> return $ Just g'
    _        -> return Nothing

playTurn :: GameInfo -> IO (Maybe RunGame)
playTurn gi@(GameInfo ps _ (Game pt dl b)) = do
  epocht <- getPOSIXTime
  case checkGameStatus (round epocht * 1000) gi of
    Nothing -> do 
      hexx <- hexxagonRunInputT (playerToHexagon pt) b
      case hexx of
        Just (Hexx _ _ fm _ _ w) -> do
          epocht' <- getPOSIXTime
          case w of
            Just Red   -> return . Just . GameOver $ hexagonToPlayer ps Red
            Just Blue  -> return . Just . GameOver $ hexagonToPlayer ps Blue
            Just Empty -> return $ Just Draw
            _ -> if   round epocht' * 1000 > dl 
                 then return $ Just TimeOut 
                 else return . Just $ PlayTurn fm
        _ -> return Nothing
    _ -> return $ checkGameStatus (round epocht * 1000) gi

resetCursor :: IO ()
resetCursor = do
  clearScreen
  setCursorPosition 0 0
  hFlush stdout

saveGame :: (Hexagon, Board) -> String -> InputT IO ()
saveGame g s = do
  g' <- liftIO . try . writeFile (last $ words s) . show $ destructureGame g :: InputT IO (Either IOException ())
  case g' of
    Right _ -> return ()
    _ -> return ()

scoreIO :: Board -> InputT IO ()
scoreIO b = outputStrLn . (\(r,bl) -> "\n   " <> (if r == 0 then "\n" else show r <> "\n   ") <> concatMap colorRed (replicate r ' ') <> "\n   " <> (if bl == 0 then "" else show bl) <> "\n   " <> concatMap colorBlue (replicate bl ' ')) $ score b

screenDefault :: Hexx -> InputT IO ()
screenDefault (Hexx b _ _ _ pt _) = do
  liftIO resetCursor
  titleDefault pt
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords []) b
  return ()

screenGameHelp :: InputT IO ()
screenGameHelp = do
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

screenWinner :: Hexx -> InputT IO ()
screenWinner (Hexx b _ _ _ _ w) = do
  liftIO resetCursor
  titleWinner w
  scoreIO b
  outputStrLn . concatMap colorize $ showBoard Edge (SelectiveCoords []) b
  return ()

titleDefault :: Hexagon -> InputT IO ()
titleDefault h = 
  if   h == Red 
  then outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull C.Red  , SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]
  else outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN " <> setSGRCode [Reset]

titleWinner :: Maybe Hexagon -> InputT IO ()
titleWinner w = case w of
  Just Red  -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Dull  C.Red ,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  Just Blue -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Blue,  SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN WINNER! " <> setSGRCode [Reset]
  _         -> outputStrLn $ "\n  " <> setSGRCode [SetColor Background Vivid C.Black, SetConsoleIntensity BoldIntensity] <> " HEXXAGŌN TIE! " <> setSGRCode [Reset]

--testGameInfo :: GameInfo
--testGameInfo = 
--  GameInfo 
--    [ RedPlayer  "6373" "tnr"
--    , BluePlayer "6373" "tnb"
--    ] 
--    3600000 $ 
--    Game 
--      (BluePlayer "6373" "tnb")
--      1911743228000 $ 
--      listToBoard $ read "[((1,1),'e'),((1,2),'e'),((1,3),'e'),((2,1),'e'),((2,2),'e'),((2,3),'e'),((2,4),'e'),((3,1),'e'),((3,2),'e'),((3,3),'e'),((3,4),'e'),((3,5),'e'),((4,2),'e'),((4,3),'e'),((4,4),'e'),((4,5),'e'),((5,3),'e'),((5,4),'e'),((5,5),'e')]"
