{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where
import Test.QuickCheck (generate)
import DataTypes (Hexagon, Position, Board (Board))
import Arbitrary ( arbitrary )
import Data.Char (chr)
import qualified Data.Map as Map

main :: IO ()
main = do
    generate (arbitrary @Hexagon) >>= print
    generate (arbitrary @Position) >>= print
    board@(Board mapBoard) <-generate (arbitrary @Board)
    print mapBoard
    print board
    print (length $ Map.keys mapBoard)

