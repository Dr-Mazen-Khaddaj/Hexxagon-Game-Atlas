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
import  Data.Map            qualified as Map

------------------------------------------------------ | Data Types | ------------------------------------------------------
--  Player
data    Player          = RedPlayer  CurrencySymbol TokenName
                        | BluePlayer CurrencySymbol TokenName
                        deriving stock Eq

--  Board
data    Hexagon         = Empty
                        | Red
                        | Blue
                        deriving stock Eq

data    Position        = Position  { getX :: Int
                                    , getY :: Int
                                    }
                        deriving stock (Show, Ord, Eq)

type    Block           = (Position,Hexagon)

newtype Board           = Board (Map.Map Position Hexagon)
                        deriving newtype Eq

--  Move
data    Move            = Move      { initialPosition   :: Position
                                    , finalPosition     :: Position
                                    }

--  Game
data    GameSettings    = Settings  { getPlayer1        :: Player
                                    , getTurnDuration   :: POSIXTime
                                    , getBoardS0        :: Board
                                    }

data    GameState       = Game      { getPlayer'sTurn   :: Player
                                    , getDeadline       :: POSIXTime
                                    , getBoard          :: Board
                                    }

data    GameInfo        = GameInfo  { getPlayers        :: [Player]
                                    , getTurnDuration'  :: POSIXTime
                                    , getGameState      :: GameState
                                    }

data    Initialization  = Add Player
                        | Withdraw

data    RunGame         = PlayTurn
                        | GameOver Player
                        | TimeOut

----------------------------------------------------------------------------------------------------------------------------