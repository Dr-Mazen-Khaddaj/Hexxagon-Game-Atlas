{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test_UtilityFxs where

import UtilityFxs
import Test.QuickCheck (generate, Arbitrary (arbitrary))
import DataTypes (Board (Board), Move (Move), Position (Position), Hexagon (..), Block)
import qualified Text.Read as Text
import Arbitrary ()
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Data.Char (toLower)
import qualified PlutusTx.AssocMap as AssocMap
import Data.Maybe (fromJust)
import Constants (classicBoard_S9DC3)
import Test_Instances (printPassOrFail, printUnderlined)
import Control.Exception (try, ErrorCall (ErrorCallWithLocation), evaluate)

testMakeMove :: IO ()
testMakeMove = do
    let move1  = Move (Position 1 1) (Position 2 2)
    let move2a = Move (Position 1 1) (Position 1 3)
    let move2b = Move (Position 1 3) (Position 2 5)
    let move3  = Move (Position 1 1) (Position 4 4)
    let move4  = Move (Position 1 6) (Position 2 6)
    let move5  = Move (Position 2 1) (Position 2 2)
    print $ makeMove move1 classicBoard_S9DC3
    print $ makeMove move2a classicBoard_S9DC3
    print $ foldr makeMove classicBoard_S9DC3 [move2b, move2a]
    Left (ErrorCallWithLocation e1 _) <- try @ErrorCall (evaluate $ makeMove move3 classicBoard_S9DC3)
    Left (ErrorCallWithLocation e2 _) <- try @ErrorCall (evaluate $ makeMove move4 classicBoard_S9DC3)
    Left (ErrorCallWithLocation e3 _) <- try @ErrorCall (evaluate $ makeMove move5 classicBoard_S9DC3)
    mapM_ print [e1,e2,e3]

testMakeBoard :: Integer -> IO ()
testMakeBoard n = do
    modifications <- generate $ arbitrary @[Either Position Block]
    deletions     <- generate $ arbitrary @[Position]
    print $ makeEmptyBoard          n
    print $ makeEmptyClassicBoard   n
    print $ makeClassicBoard        n modifications
    print $ makeStartingBoard       n deletions
    print classicBoard_S9DC3

-- Automatic Sample Test
testAllUtilityFxsSample :: IO ()
testAllUtilityFxsSample = do
    printUnderlined "Testing Utility Functions @UtilityFxs"
    printPassOrFail $ distance (Move (Position 2 3) (Position 4 4)) == 2
    printPassOrFail $ getNearbyPositions classicBoard_S9DC3 (Position 2 3) Blue 2 == [Position 1 1]
    printPassOrFail $ calculateNearbyPositions (Position 2 3) 1 == [Position 3 3, Position 3 4, Position 2 4, Position 2 2, Position 1 2, Position 1 3]

-- Manual Test
testAllUtilityFxsManual :: IO ()
testAllUtilityFxsManual = do
    putStrLn "\nTesting Utility Functions @UtilityFxs"
    board       <- generate $ arbitrary @Board
    testUtilityFxs board

testUtilityFxs :: Board -> IO ()
testUtilityFxs board = do
    testNumber  <- getTestNumber $ zip [1..] ["distance","getNearbyPositions","calculateNearbyPositions"]
    case testNumber of
        1 -> test_distance
        2 -> test_getNearbyPositions board
        3 -> test_calculateNearbyPositions
        _ -> error "Invalid Test Number"
    putStr "Another Test (Y/N) ? "
    hFlush stdout
    answer <- askYesNo
    when answer $ testUtilityFxs board

-- Individual Tests

test_distance :: IO ()
test_distance = do
    print classicBoard_S9DC3
    move <- getMove
    putStr "Distance : "
    print $ distance move

test_getNearbyPositions :: Board -> IO ()
test_getNearbyPositions board@(Board boardMap) = do
    print board
    putStrLn "Enter Position, Hexagon, and Distance"
    position  <- getPosition
    hexagon   <- getHexagon
    distance  <- getInteger
    let positions = getNearbyPositions board position hexagon distance
    putStrLn "Positions : "
    print positions
    print $ Board . AssocMap.fromList $ (\ pos -> (pos, fromJust $ AssocMap.lookup pos boardMap)) <$> positions

test_calculateNearbyPositions :: IO ()
test_calculateNearbyPositions = do
    putStrLn "Enter Position, and Distance"
    position  <- getPosition
    distance  <- getInteger
    let positions = calculateNearbyPositions position distance
    putStrLn "Positions : "
    print positions
    print $ Board $ AssocMap.fromList $ (,Empty) <$> positions


-- Helper Functions

getTestNumber :: [(Integer,String)] -> IO Integer
getTestNumber tests = do
    let printTest (n,name) = putStrLn $ show n <> " - " <> name
    mapM_ printTest tests
    putStrLn "----------------------"
    putStr "Please Choose a Test : "
    hFlush stdout
    n <- getInteger
    if any ((== n) . fst) tests
        then pure n
        else getTestNumber tests

getPosition :: IO Position
getPosition = Position <$> getInteger <*> getInteger

getMove :: IO Move
getMove = do
    putStrLn "Enter Position 1 :" ; p1 <- getPosition
    putStrLn "Enter Position 2 :" ; Move p1 <$> getPosition

getHexagon :: IO Hexagon
getHexagon = do
    input <- getLine
    case toLower <$> input of
        "b"         -> return Blue
        "r"         -> return Red
        "-"         -> return Empty
        _           -> putStrLn "Invalid Answer !" >> getHexagon

-- IO Tools

getInteger :: IO Integer
getInteger = do
    input <- Text.readMaybe <$> getLine
    case input of
        Just n -> pure n
        Nothing -> putStrLn "Please enter a valid Number!" >> getInteger

askYesNo :: IO Bool
askYesNo = do
    answer <- getLine
    case toLower <$> answer of
        "yes"   -> return True
        "y"     -> return True
        "no"    -> return False
        "n"     -> return False
        _       -> putStrLn "Invalid Answer !" >> askYesNo