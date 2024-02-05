{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module  Test_Instances  ( testInstances
                        , testAllInstances
                        ) where

import  DataTypes
import  Arbitrary           ()
import  Test.QuickCheck     (generate, Arbitrary (arbitrary), vectorOf)
import  PlutusTx.Prelude    qualified as PlutusTx
import  PlutusTx.Show       qualified as PlutusTx
import  Data.Text           qualified as Text
import  PlutusTx.Builtins   (fromBuiltin)

testInstances   ::  ( Arbitrary a
                    , Eq a
                    , PlutusTx.Eq a
                    , Show a
                    , PlutusTx.Show a
                    ) => IO a
testInstances = do
    as <- generate . vectorOf 3 $ arbitrary
    let compareWith f = [f a1 a2 | a1 <- as, a2 <- as]
    let a = head as

    putStr "Compare With (==)           : " >> print ( compareWith (==)           )
    putStr "Compare With (PlutusTx.==)  : " >> print ( compareWith (PlutusTx.==)  )
    printPassOrFail $ compareWith (==) == compareWith (PlutusTx.==)

    putStr "Show with (show)            : " >> print a
    putStr "Show with (PlutusTx.show)   : " >> putStrLn (Text.unpack . fromBuiltin $ PlutusTx.show a)
    printPassOrFail $ show a == Text.unpack (fromBuiltin $ PlutusTx.show a)

    pure a

printPassOrFail :: Bool -> IO ()
printPassOrFail True  = putStrLn $ "\ESC[32m" <> "-- Pass --" <> "\ESC[0m"
printPassOrFail False = putStrLn $ "\ESC[31m" <> "-- Fail --" <> "\ESC[0m"

-- Run Tests
testAllInstances :: IO ()
testAllInstances = do
    printUnderlined "Testing Instances of Player"           >> testInstances @Player
    printUnderlined "Testing Instances of Hexagon"          >> testInstances @Hexagon
    printUnderlined "Testing Instances of Position"         >> testInstances @Position
    printUnderlined "Testing Instances of Block"            >> testInstances @Block
    printUnderlined "Testing Instances of Move"             >> testInstances @Move
    printUnderlined "Testing Instances of Board"            >> testInstances @Board
    printUnderlined "Testing Instances of Initialization"   >> testInstances @Initialization
    printUnderlined "Testing Instances of GameSettings"     >> testInstances @GameSettings
    printUnderlined "Testing Instances of GameState"        >> testInstances @GameState
    printUnderlined "Testing Instances of GameInfo"         >> testInstances @GameInfo
    printUnderlined "Testing Instances of RunGame"          >> testInstances @RunGame
    putStrLn "Done"

printUnderlined :: String -> IO ()
printUnderlined str = putStrLn $ "\x1b[4m" <> str <> "\x1b[0m"