module Main (main) where

import Test.QuickCheck (generate, Arbitrary (arbitrary))
import DataTypes
import Arbitrary ()
import qualified PlutusTx.AssocMap as AssocMap
import UtilityFxs (makeEmptyBoard, makeEmptyClassicBoard, makeStartingBoard)
import Constants (classicBoard_S9DC3)
import Instances (Orientation(..), BoardMode (..), showBoard)
import Test_Instances (testAllInstances)

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
    testAllInstances

-- Testing
-- runTestArbitrary :: IO ()
-- runTestArbitrary = do
--     undefined
    -- sample $ arbitrary @Value
    -- sample $ encode . fromBuiltin <$> genHash
    -- sample $ encode . fromBuiltin <$> genSizedBuiltinByteString 28
    -- sample $ (\tn@(TokenName bs) -> (tn, encode $ fromBuiltin bs)) <$> arbitrary @TokenName

