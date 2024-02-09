module Test_Arbitrary where

import DataTypes
import Test.QuickCheck (generate, Arbitrary (arbitrary))
import Arbitrary ()

testArbitraryDataTypes :: IO ()
testArbitraryDataTypes = do

    generate (arbitrary @Player)            >>= print
    generate (arbitrary @Hexagon)           >>= print
    generate (arbitrary @Position)          >>= print
    generate (arbitrary @Move)              >>= print
    generate (arbitrary @Board)             >>= print
    generate (arbitrary @GameSettings)      >>= print
    generate (arbitrary @GameState)         >>= print
    generate (arbitrary @GameInfo)          >>= print
    generate (arbitrary @Initialization)    >>= print
    generate (arbitrary @RunGame)           >>= print

    pure ()