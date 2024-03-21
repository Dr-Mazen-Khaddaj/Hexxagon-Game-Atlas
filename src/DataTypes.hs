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
                    , Metadata          (..)
                    ) where

import  PlutusLedgerApi.V2  (CurrencySymbol, TokenName, POSIXTime, BuiltinByteString, Data)
import  PlutusTx.AssocMap   qualified as AssocMap

------------------------------------------------------ | Data Types | ------------------------------------------------------
--  Player
data    Player          = RedPlayer  CurrencySymbol TokenName
                        | BluePlayer CurrencySymbol TokenName
                        deriving stock (Eq, Show)

--  Board
data    Hexagon         = Empty
                        | Red
                        | Blue
                        deriving stock Eq

data    Position        = Position  { getX :: Integer
                                    , getY :: Integer
                                    }
                        deriving stock (Show, Ord, Eq)

type    Block           = (Position,Hexagon)

newtype Board           = Board (AssocMap.Map Position Hexagon)
                        deriving stock Eq

--  Move
data    Move            = Move      { initialPosition   :: Position
                                    , finalPosition     :: Position
                                    } deriving stock (Eq, Show)

--  Game
data    GameSettings    = Settings  { getPlayer1        :: Player
                                    , getTurnDuration   :: POSIXTime
                                    , getBoardS0        :: Board
                                    } deriving stock Eq

data    GameState       = Game      { getPlayer'sTurn   :: Player
                                    , getDeadline       :: POSIXTime
                                    , getBoard          :: Board
                                    } deriving stock Eq

data    GameInfo        = GameInfo  { getPlayers        :: [Player]
                                    , getTurnDuration'  :: POSIXTime
                                    , getGameState      :: GameState
                                    } deriving stock Eq

data    Initialization  = Add Player
                        | Withdraw
                        deriving stock (Eq, Show)

data    RunGame         = PlayTurn Move
                        | GameOver Player
                        | Draw
                        | TimeOut
                        deriving stock (Eq, Show)

-- Metadata
data Metadata           = Metadata  { getMetadata       :: AssocMap.Map BuiltinByteString Data
                                    , getVersionNum     :: Integer
                                    , getExtraData      :: Data
                                    }

----------------------------------------------------------------------------------------------------------------------------