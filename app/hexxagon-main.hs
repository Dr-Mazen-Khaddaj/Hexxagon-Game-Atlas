module Main where

import DataTypes 
  ( Hexx(..)
  , Hexagon(..)
  , Move(..)
  , Position(..)
  )
import IOFxs 
  ( checkWinnerIO
  )
import System.Console.Haskeline
import Constants
  ( classicBoard_S9DC3
  )

main :: IO (Maybe Hexx)
main = runInputT defaultSettings $ checkWinnerIO $ Hexx Red (Move (Position (-1) (-1)) (Position (-1) (-1))) Nothing False classicBoard_S9DC3
