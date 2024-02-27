module Main (main) where

import Arbitrary        ()
import Test_Instances   (testAllInstances)
import Test_UtilityFxs  (testAllUtilityFxsSample, testMakeBoard, testMakeMove, testBotPlayGame)
import Test_Arbitrary   (testArbitraryDataTypes)
import System.IO        (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    testArbitraryDataTypes
    testMakeBoard 5
    testAllUtilityFxsSample
    testAllInstances False
    testMakeMove
    testBotPlayGame
    -- testAllUtilityFxsManual