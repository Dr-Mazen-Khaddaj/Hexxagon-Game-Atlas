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

                    , showBoard
                    , Orientation       (..)
                    , BoardMode         (..)
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
    show = showBoard Edge CoordsAndHexs

---- Show Board (Vertex Orientation) ----       ---- Show Board (Edge Orientation) ---- __    __    __    __
--  / \ / \ / \ / \ / \ / \ / \ / \ / \ / \     -- /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \
-- |   |   |   |   |   |   |   |   |   |   |    -- \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
--  \ / \ / \ / \ / \ / \ / \ / \ / \ / \ / \   --    \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
--   |   |   |   |   |   |   |   |   |   |   |  --    /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \
--    \ / \ / \ / \ / \ / \ / \ / \ / \ / \ /   --    \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/

showBoard :: Orientation -> BoardMode -> Board -> String
showBoard orientation mode = showOrganizedBoard orientation mode . organizeBoard orientation

data Orientation = Vertex | Edge
data BoardMode = Clear | OnlyHexagons | OnlyCoordinates | CoordsAndHexs
data Side = Top | Middle | Bottom

organizeBoard :: Orientation -> Board -> [[Maybe (Position,Hexagon)]]
organizeBoard Vertex = (organizeLine <$>) . fillBoard . groupBoard . sortBoard . destructureBoard
    where
        groupBoard = List.groupBy ((==) `on` getY . fst)
        sortBoard = List.sortOn (Down . getY . fst)
        fillBoard (xH:xs@(xL:_)) = xH : gap <> fillBoard xs
            where   getLevel = getY . fst . head
                    gap = replicate (getLevel xH - getLevel xL - 1) []
        fillBoard list = list

organizeBoard Edge = (organizeLine <$>) . List.reverse . buildBoard 0 . destructureBoard
    where
        buildBoard _ [] = []
        buildBoard n (buildLevel n -> (levelN,rest)) = levelN : buildBoard (n+1) rest
        buildLevel n = List.partition (isAtLevel n)
        isAtLevel level = (level ==) . (\pos -> pos.getX + pos.getY) . fst

destructureBoard :: Board -> [(Position, Hexagon)]
destructureBoard (Board boardMap) = Map.toList boardMap

organizeLine :: [(Position, b)] -> [Maybe (Position, b)]
organizeLine = fillLine 0 . List.sortOn (getX . fst)
    where
        fillLine i (xL:xs) = if getX (fst xL) == i  then Just xL : fillLine (i+1) xs
                                                    else Nothing : fillLine (i+1) (xL:xs)
        fillLine _ [] = []


showOrganizedBoard :: Orientation -> BoardMode -> [[Maybe (Position,Hexagon)]] -> String
showOrganizedBoard orientation mode = case orientation of
    Edge    -> concatLines . pruneBoard . (showLine <$>) . zip [1..]
    Vertex  -> concatLines              . (showLine <$>) . zip [1..]
    where
        pruneBoard board = (pruneLine (length board * 2) <$>) <$> board
        pruneLine l (c:cs) = c: drop l cs
        pruneLine _ [] = []

        showLine :: (Int, [Maybe (Position,Hexagon)]) -> [String]
        showLine (n, sortedLine) = (indentation <>) <$> (($ sortedLine) <$> [ concatMap (showHex Top)
                                                                            , concatMiddle . (showHex Middle <$>)
                                                                            , concatMap (showHex Bottom)
                                                                            ])
            where
                indentation = '\n' : replicate (hexSize*n) ' '
                showHex Top    (Just _)     = topHex
                showHex Middle (Just (p,h)) = middleHex p h
                showHex Bottom (Just (_,h)) = case (orientation,mode) of
                                                (Edge,CoordsAndHexs)  -> bottomWithHex h
                                                _                     -> bottomHex
                showHex _      Nothing      = noHex
                concatMiddle (m1:m2:ms) = if last m1 == '|' then m1 <> concatMiddle (tail m2 : ms)
                                                            else m1 <> concatMiddle (m2 : ms)
                concatMiddle ms = concat ms

                (topHex,bottomHex,noHex) = case orientation of
                    Edge    -> (" ___    " , "\\___/   " , "        ")
                    Vertex  -> (" / \\" , " \\ /" , "    ")
                middleHex pos hex = case orientation of
                    Edge    -> case mode of
                        Clear           -> "/   \\   "
                        OnlyHexagons    -> "/ " <> show hex <> " \\   "
                        OnlyCoordinates -> "/" <> show (mod pos.getX 10) <> "," <> show (mod pos.getY 10) <> "\\   "
                        CoordsAndHexs   -> "/" <> show (mod pos.getX 10) <> " " <> show (mod pos.getY 10) <> "\\   "
                    Vertex  -> case mode of
                        Clear           -> "|   |"
                        OnlyHexagons    -> "| " <> show hex <> " |"
                        OnlyCoordinates -> "|" <> show (mod pos.getX 10) <> "," <> show (mod pos.getY 10) <> "|"
                        CoordsAndHexs   -> "|" <> show (mod pos.getX 10) <> show hex <> show (mod pos.getY 10) <> "|"
                bottomWithHex hex = case hex of
                    Empty   -> "\\___/   "
                    _       -> "\\_" <> show hex <> "_/   "
                hexSize = case orientation of
                    Edge    -> 4
                    Vertex  -> 2

        concatLines :: [[String]] -> String
        concatLines (line1 : line2 : ls) = case orientation of
            Edge    -> head line1 <> concatLines ([ line1 !! 1 `appendLine` head line2 , line1 !! 2 `appendLine` (line2 !! 1) , line2 !! 2] : ls)
            Vertex  -> head line1 <> line1 !! 1 <> concatLines ([line1 !! 2 `appendLine` head line2 , line2 !! 1 , line2 !! 2] : ls)
            where
                appendLine (c1:cs1) (c2:cs2)    | c1 == ' ' = c2 : appendLine cs1 cs2
                                                | c2 == ' ' = c1 : appendLine cs1 cs2
                                                | otherwise = c1 : appendLine cs1 cs2
                appendLine [] cs = cs
                appendLine cs [] = cs
        concatLines ls = concat $ concat ls