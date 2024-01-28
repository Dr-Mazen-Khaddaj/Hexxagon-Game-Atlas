module  DataTypes   ( Player            (..)
                    , Hexagon           (..)
                    , Position          (..)
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
import  Data.List           qualified as List
import  Data.Function       (on)
import  Data.Ord            (Down(Down))

--  Player
data    Player          = RedPlayer  CurrencySymbol TokenName
                        | BluePlayer CurrencySymbol TokenName

--  Board
data    Hexagon         = Empty
                        | Red
                        | Blue

data    Position        = Position  { getX :: Int
                                    , getY :: Int
                                    }
                        deriving (Show, Ord, Eq)

newtype Board           = Board (Map.Map Position Hexagon)

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

-- | Show Instance | --

instance Show Hexagon where show :: Hexagon -> String
                            show Empty  = "-"
                            show Red    = "R"
                            show Blue   = "B"

instance Show Board where
    show :: Board -> String
    show = showBoard Vertex . organizeBoard

---- | Show Board | ----

data Orientation = Vertex | Edge
data Side = Top | Middle | Bottom

organizeBoard :: Board -> [[Maybe (Position,Hexagon)]]
organizeBoard = (sortLine <$>) . fillBoard . destructure
    where
            destructure (Board boardMap) = List.groupBy ((==) `on` getY . fst) . sortBoard $ Map.toList boardMap
            sortBoard = List.sortOn (Down . getY . fst)
            fillBoard (xH:xs@(xL:_)) = xH : gap <> fillBoard xs
                where   getLevel = getY . fst . head
                        gap = replicate (getLevel xH - getLevel xL - 1) []
            fillBoard list = list

            sortLine = fillLine 0 . List.sortOn (getX . fst)
            fillLine i (xL:xs) = if getX (fst xL) == i  then Just xL : fillLine (i+1) xs
                                                        else Nothing : fillLine (i+1) (xL:xs)
            fillLine _ [] = []

showBoard :: Orientation -> [[Maybe (Position,Hexagon)]] -> String

---- Show Board (Vertex Orientation) ----
--  / \ / \ / \ / \ / \ / \ / \ / \ / \ / \
-- |   |   |   |   |   |   |   |   |   |   |
--  \ / \ / \ / \ / \ / \ / \ / \ / \ / \ / \
--   |   |   |   |   |   |   |   |   |   |   |
--    \ / \ / \ / \ / \ / \ / \ / \ / \ / \ /
showBoard Vertex = concatLines . (showLine <$>) . zip [1..]
    where
        showLine :: (Int, [Maybe (Position,Hexagon)]) -> [String]
        showLine (n, sortedLine) = (indentation <>) <$> (($ sortedLine) <$> [ concatMap (showHex Top)
                                                                            , concatMiddle . (showHex Middle <$>)
                                                                            , concatMap (showHex Bottom)
                                                                            ])
            where
                indentation = '\n' : replicate (2*n) ' '
                showHex Top    (Just _)     = " / \\"
                -- showMiddle (Just (p,h)) = "|" <> show (mod p.getX 10) <> show h <> show (mod p.getY 10) <> "|"
                showHex Middle (Just (_,h)) = "| " <> show h <> " |"
                showHex Bottom (Just _)     = " \\ /"
                showHex _    Nothing      = "    "
                concatMiddle (m1:m2:ms) = if last m1 == '|' then m1 <> concatMiddle (tail m2 : ms)
                                                            else m1 <> concatMiddle (m2 : ms)
                concatMiddle ms = concat ms

        concatLines :: [[String]] -> String
        concatLines (line1 : line2 : ls) = head line1 <> line1 !! 1 <> concatLines ([line1 !! 2 `appendLine` head line2 , line2 !! 1 , line2 !! 2] : ls)
            where
                appendLine (c1:cs1) (c2:cs2)    | c1 == ' ' = c2 : appendLine cs1 cs2
                                                | c2 == ' ' = c1 : appendLine cs1 cs2
                                                | otherwise = c1 : appendLine cs1 cs2
                appendLine [] cs = cs
                appendLine cs [] = cs
        concatLines ls = concat $ concat ls

---- Show Board (Edge Orientation) ---- __    __    __    __
-- /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \
-- \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
--    \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
--    /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \
--    \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
--       \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
showBoard Edge = undefined

transposeBoard :: [] [Maybe (Position,Hexagon)] -> [] [Maybe (Position,Hexagon)]
transposeBoard = undefined