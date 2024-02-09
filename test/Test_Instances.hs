{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module  Test_Instances  ( testInstances
                        , testAllInstances
                        , testShowBoard
                        , printPassOrFail
                        , printUnderlined
                        ) where

import  DataTypes
import  Arbitrary           ()
import  Control.Monad       (when)
import  Test.QuickCheck     (generate, Arbitrary (arbitrary), vectorOf)
import  PlutusTx.Prelude    qualified as PlutusTx
import  PlutusTx.Show       qualified as PlutusTx
import  Data.Text           qualified as Text
import  PlutusTx.Builtins   (fromBuiltin)
import  Constants           (classicBoard_S9DC3)
import  Instances           (showBoard, Orientation (..), BoardMode (..))

testInstances   ::  ( Arbitrary a
                    , Eq a
                    , PlutusTx.Eq a
                    , Show a
                    , PlutusTx.Show a
                    ) => Bool -> IO a
testInstances showDetails = do
    as <- generate . vectorOf 3 $ arbitrary
    let compareWith f = [f a1 a2 | a1 <- as, a2 <- as]
    let a = head as

    when showDetails $ do
        putStr "Compare With (==)           : " >> print ( compareWith (==)           )
        putStr "Compare With (PlutusTx.==)  : " >> print ( compareWith (PlutusTx.==)  )
    printPassOrFail $ compareWith (==) == compareWith (PlutusTx.==)

    when showDetails $ do
        putStr "Show with (show)            : " >> print a
        putStr "Show with (PlutusTx.show)   : " >> putStrLn (Text.unpack . fromBuiltin $ PlutusTx.show a)
    printPassOrFail $ show a == Text.unpack (fromBuiltin $ PlutusTx.show a)

    pure a

printPassOrFail :: Bool -> IO ()
printPassOrFail True  = putStrLn $ "\ESC[32m" <> "-- Pass --" <> "\ESC[0m"
printPassOrFail False = putStrLn $ "\ESC[31m" <> "-- Fail --" <> "\ESC[0m"

-- Run Tests
testAllInstances :: Bool -> IO ()
testAllInstances showDetails = do
    printUnderlined "Testing Instances of Player"           >> testInstances @Player            showDetails
    printUnderlined "Testing Instances of Hexagon"          >> testInstances @Hexagon           showDetails
    printUnderlined "Testing Instances of Position"         >> testInstances @Position          showDetails
    printUnderlined "Testing Instances of Block"            >> testInstances @Block             showDetails
    printUnderlined "Testing Instances of Move"             >> testInstances @Move              showDetails
    printUnderlined "Testing Instances of Board"            >> testInstances @Board             showDetails
    printUnderlined "Testing Instances of Initialization"   >> testInstances @Initialization    showDetails
    printUnderlined "Testing Instances of GameSettings"     >> testInstances @GameSettings      showDetails
    printUnderlined "Testing Instances of GameState"        >> testInstances @GameState         showDetails
    printUnderlined "Testing Instances of GameInfo"         >> testInstances @GameInfo          showDetails
    printUnderlined "Testing Instances of RunGame"          >> testInstances @RunGame           showDetails
    pure ()

printUnderlined :: String -> IO ()
printUnderlined str = putStrLn $ "\x1b[4m" <> str <> "\x1b[0m"

testShowBoard :: IO ()
testShowBoard = sequence_   [ putStrLn $ showBoard orientation mode classicBoard_S9DC3
                            | orientation   <- [Vertex,Edge]
                            , mode          <- [Clear,OnlyCoordinates,OnlyHexagons,CoordsAndHexs]
                            ]