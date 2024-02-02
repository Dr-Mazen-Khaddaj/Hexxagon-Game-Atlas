{-# LANGUAGE TemplateHaskell #-}

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
import  Data.Maybe          (fromJust)
import  Data.Map            qualified as Map
import  PlutusTx.IsData     qualified as PlutusTx
import  PlutusTx.AssocMap   qualified as AssocMap

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

data    Position        = Position  { getX :: Integer
                                    , getY :: Integer
                                    }
                        deriving stock (Show, Ord, Eq)
PlutusTx.makeIsDataIndexed ''Position [('Position,0)]

type    Block           = (Position,Hexagon)

instance PlutusTx.ToData (Map.Map Position Hexagon) where toBuiltinData = PlutusTx.toBuiltinData . AssocMap.fromList . Map.toList
instance PlutusTx.FromData (Map.Map Position Hexagon) where fromBuiltinData = (Map.fromList . AssocMap.toList <$>) . PlutusTx.fromBuiltinData
instance PlutusTx.UnsafeFromData (Map.Map Position Hexagon) where unsafeFromBuiltinData = fromJust . PlutusTx.fromBuiltinData

newtype Board           = Board (Map.Map Position Hexagon)
                        deriving newtype Eq
PlutusTx.makeIsDataIndexed ''Board [('Board, 0)]

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
                        deriving (Eq, Show)
PlutusTx.makeIsDataIndexed ''Initialization [('Add,0),('Withdraw,1)]

data    RunGame         = PlayTurn
                        | GameOver Player
                        | TimeOut
                        deriving (Eq, Show)
PlutusTx.makeIsDataIndexed ''RunGame [('PlayTurn,0),('GameOver,1),('TimeOut,2)]

----------------------------------------------------------------------------------------------------------------------------