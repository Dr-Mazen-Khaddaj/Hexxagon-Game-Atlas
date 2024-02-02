{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module  DataTypes   ( Player            (..)
                    , Hexagon           (..)
                    , Position          (..)
                    , Block
                    , Move              (..)
                    , Board             (..)
                    , Initialization    (..)
                    , GameSettings      (..)
                    , GameState         (..)
                    , GameInfo          (..)
                    , RunGame           (..)
                    ) where

import  PlutusLedgerApi.V2  (CurrencySymbol, TokenName, POSIXTime)
import  PlutusTx.IsData     qualified as PlutusTx
import  PlutusTx.AssocMap   qualified as AssocMap
import  PlutusTx.Prelude    qualified as PlutusTx
import  PlutusTx.Show       qualified as PlutusTx

------------------------------------------------------ | Data Types | ------------------------------------------------------
--  Player
data    Player          = RedPlayer  CurrencySymbol TokenName
                        | BluePlayer CurrencySymbol TokenName
                        deriving stock (Eq, Show)
PlutusTx.makeIsDataIndexed ''Player [('RedPlayer, 0),('BluePlayer, 1)]

--  Board
data    Hexagon         = Empty
                        | Red
                        | Blue
                        deriving stock Eq
PlutusTx.makeIsDataIndexed ''Hexagon [('Empty, 0),('Red, 1),('Blue, 2)]

instance PlutusTx.Eq Hexagon where  Empty == Empty  = True
                                    Red   == Red    = True
                                    Blue  == Blue   = True
                                    _     == _      = False

data    Position        = Position  { getX :: Integer
                                    , getY :: Integer
                                    }
                        deriving stock (Show, Ord, Eq)
                        deriving anyclass PlutusTx.Show
PlutusTx.makeIsDataIndexed ''Position [('Position,0)]

instance PlutusTx.Eq Position where (Position x1 y1) == (Position x2 y2) = x1 == x2 && y1 == y2

type    Block           = (Position,Hexagon)

newtype Board           = Board (AssocMap.Map Position Hexagon)
                        deriving stock Eq
PlutusTx.makeIsDataIndexed ''Board [('Board, 0)]

instance PlutusTx.Eq Board where (Board b1) == (Board b2) = b1 == b2

--  Move
data    Move            = Move      { initialPosition   :: Position
                                    , finalPosition     :: Position
                                    } deriving stock (Eq, Show)
PlutusTx.makeIsDataIndexed ''Move [('Move,0)]

--  Game
data    GameSettings    = Settings  { getPlayer1        :: Player
                                    , getTurnDuration   :: POSIXTime
                                    , getBoardS0        :: Board
                                    } deriving stock Eq
PlutusTx.makeIsDataIndexed ''GameSettings [('Settings,0)]

data    GameState       = Game      { getPlayer'sTurn   :: Player
                                    , getDeadline       :: POSIXTime
                                    , getBoard          :: Board
                                    } deriving stock Eq
PlutusTx.makeIsDataIndexed ''GameState [('Game,0)]

data    GameInfo        = GameInfo  { getPlayers        :: [Player]
                                    , getTurnDuration'  :: POSIXTime
                                    , getGameState      :: GameState
                                    } deriving stock Eq
PlutusTx.makeIsDataIndexed ''GameInfo [('GameInfo,0)]

data    Initialization  = Add Player
                        | Withdraw
                        deriving stock (Eq, Show)
PlutusTx.makeIsDataIndexed ''Initialization [('Add,0),('Withdraw,1)]

data    RunGame         = PlayTurn
                        | GameOver Player
                        | TimeOut
                        deriving stock (Eq, Show)
PlutusTx.makeIsDataIndexed ''RunGame [('PlayTurn,0),('GameOver,1),('TimeOut,2)]

----------------------------------------------------------------------------------------------------------------------------