module Main where

import DataTypes 
  ( Hexx(..)
  , Hexagon(..)
  , Move(..)
  , Position(..)
  )

import IOFxs 
  ( configBoard
  )
import Constants
  ( classicBoard_S9DC3
  )

main :: IO ()
main = configBoard $ Hexx Red (Move (Position (-1) (-1)) (Position (-1) (-1))) Nothing True classicBoard_S9DC3

