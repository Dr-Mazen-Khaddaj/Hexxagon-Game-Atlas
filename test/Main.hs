{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where
import Test.QuickCheck (generate)
import DataTypes (Hexagon (..), Position (..), Board (Board))
import Arbitrary ( arbitrary )
import Data.Char (chr)
import qualified PlutusTx.AssocMap as AssocMap
import UtilityFxs (makeEmptyBoard, makeEmptyClassicBoard, makeStartingBoard, getNearbyPositions)
import Variables (classicBoard_S9DC3)
import Instances (Orientation(..), BoardMode (..), showBoard)

main :: IO ()
main = do
    generate (arbitrary @Hexagon) >>= print
    generate (arbitrary @Position) >>= print
    -- board@(Board mapBoard) <-generate (arbitrary @Board)
    let board@(Board mapBoard) = makeEmptyBoard 5
    sequence_ [putStrLn $ showBoard orientation mode board| orientation <- [Vertex,Edge], mode <- [Clear,OnlyCoordinates,OnlyHexagons,CoordsAndHexs]]
    mapM_ (print . makeEmptyClassicBoard) [0..10]
    mapM_ (print . flip makeStartingBoard []) [2..10]
    print (length $ AssocMap.keys mapBoard)
    print classicBoard_S9DC3
    putStrLn $ showBoard Edge OnlyHexagons classicBoard_S9DC3
    print $ classicBoard_S9DC3 == classicBoard_S9DC3
    print $ Position 5 4
    print $ Position 5 5 == Position 5 3