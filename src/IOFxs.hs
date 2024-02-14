module IOFxs where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import DataTypes (Board(..), GameInfo(..), GameState(..), Hexagon(..), Move(..), Player(..), Position)
import MainFxs (checkFinalPosition, checkInitialPosition, checkWinner, makeMove, parseInput, score)
import System.Console.Haskeline
import UtilityFxs (makeStartingBoard) 

main :: GameInfo -> IO (Either String GameInfo)
main gi = runInputT defaultSettings $ main' gi

main' :: GameInfo -> InputT IO (Either String GameInfo)
main' gi@(GameInfo _ _ (Game _ _ b)) = do 
  outputStrLn "HEXXAGŌN"
  outputStrLn $ show b
  scoreIO b
  winner <- checkWinnerIO gi
  case winner of
    Right _ -> checkWinnerIO gi
    _ -> game gi

game :: GameInfo -> InputT IO (Either String GameInfo)
game gi@(GameInfo ps dt gs) = do
  ip <- getInitialPosition gs
  case ip of
    Just (ip', p, h) -> do
      fp <- getFinalPosition gs ip'
      case fp of
        Just (fp', d) -> do
          dl <- checkDL gs
          case dl of
            True -> 
              case makeMove gi ip' p h fp' d of
                Just (Game pt dl' b) -> do
                  winner <- checkWinnerIO $ GameInfo ps dt (Game pt dl' b)
                  case winner of
                    Right _ -> do; outputStrLn $ show b; scoreIO b; checkWinnerIO gi
                    _ -> do outputStrLn $ show b; scoreIO b; 
                            posixtime <- liftIO getPOSIXTime
                            return . Right . GameInfo ps dt $ Game pt (round posixtime * 1000 + dt) b
                _ -> return $ Left ""
            _ -> return $ Left "Deadline surpassed."
        _ -> return $ Left "Invalid final position."
    _ -> return $ Left "Invalid initial position."

getInitialPosition :: GameState -> InputT IO (Maybe (Position, Player, Hexagon))
getInitialPosition gs@(Game pt _ _) = do
  ip <- getInputLine $ case pt of
    RedPlayer  _ _ -> "Red initial position: "
    BluePlayer _ _ -> "Blue initial position: "
  return $ ip >>= parseInput >>= checkInitialPosition gs

getFinalPosition :: GameState -> Position -> InputT IO (Maybe (Position, Int))
getFinalPosition (Game pt _ b) ip = do
  fp <- getInputLine $ case pt of
    RedPlayer  _ _ -> "Red final position: "
    BluePlayer _ _ -> "Blue final position: "
  return $ fp >>= parseInput >>= checkFinalPosition b . Move ip

checkDL :: GameState -> InputT IO Bool
checkDL (Game _ dl _) = do 
  posixtime <- liftIO getPOSIXTime
  if round posixtime * 1000 <= dl then return True else return False

checkWinnerIO :: GameInfo -> InputT IO (Either String GameInfo)
checkWinnerIO (GameInfo ps dt (Game pt dl b)) = case checkWinner b of
  Just (Red, b') -> do; outputStrLn "\nRed is winner!\n"; return . Right $ GameInfo ps dt $ Game pt dl b'
  Just (Blue, b') -> do; outputStrLn "\nBlue is winner!\n"; return . Right $ GameInfo ps dt $ Game pt dl b'
  Just (Empty, b') -> do; outputStrLn "\nTie!"; outputStrLn ""; return . Right $ GameInfo ps dt $ Game pt dl b'
  _ -> return $ Left ""

scoreIO :: Board -> InputT IO ()
scoreIO b = outputStrLn . (\(r,bl) -> "\nR: " <> show r <> "\nB: " <> show bl <> "\n") $ score b

testGI :: GameInfo
testGI = 
  GameInfo 
    [ RedPlayer  "6373" "tnr"
    , BluePlayer "6373" "tnb"
    ] 
    3600000 $ 
    Game 
      (RedPlayer "6373" "tnr") 
      2007885704000 $ 
      makeStartingBoard 9 []
