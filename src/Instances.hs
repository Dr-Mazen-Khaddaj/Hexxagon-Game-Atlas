{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module  Instances   ( destructureBoard, showBoard
                    , Orientation       (..)
                    , BoardMode         (..)
                    ) where

import  DataTypes
import  Data.Ord                ( Down(Down) )
import  Data.Function           ( on )
import  Data.List               qualified as List
import  PlutusTx.Builtins.Class (stringToBuiltinString)
import  PlutusTx.IsData         qualified as PlutusTx
import  PlutusTx.Prelude        qualified as PlutusTx
import  PlutusTx.Show           qualified as PlutusTx
import  PlutusTx.AssocMap       qualified as AssocMap

---------------------------------------------------- | Show Instance | -----------------------------------------------------
-- Show Hexxagon
instance Show Hexagon where show :: Hexagon -> String
                            show Empty  = "-"
                            show Red    = "R"
                            show Blue   = "B"

-- Show Board
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

organizeBoard :: Orientation -> Board -> [[Maybe Block]]
organizeBoard Vertex = (organizeLine <$>) . fillBoard . groupBoard . sortBoard . destructureBoard
    where
        groupBoard = List.groupBy ((==) `on` getY . fst)
        sortBoard = List.sortOn (Down . getY . fst)
        fillBoard (xH:xs@(xL:_)) = xH : gap <> fillBoard xs
            where   getLevel = fromInteger . getY . fst . head
                    gap = replicate (getLevel xH - getLevel xL - 1) []
        fillBoard list = list

organizeBoard Edge = (organizeLine <$>) . List.reverse . buildBoard 0 . destructureBoard
    where
        buildBoard _ [] = []
        buildBoard n (buildLevel n -> (levelN,rest)) = levelN : buildBoard (n+1) rest
        buildLevel n = List.partition (isAtLevel n)
        isAtLevel level = (level ==) . (\pos -> pos.getX + pos.getY) . fst

organizeLine :: [(Position, b)] -> [Maybe (Position, b)]
organizeLine = fillLine 0 . List.sortOn (getX . fst)
    where
        fillLine i (xL:xs) = if getX (fst xL) == i  then Just xL : fillLine (i+1) xs
                                                    else Nothing : fillLine (i+1) (xL:xs)
        fillLine _ [] = []


showOrganizedBoard :: Orientation -> BoardMode -> [[Maybe Block]] -> String
showOrganizedBoard orientation mode = case orientation of
    Edge    -> concatLines . pruneBoard . (showLine <$>) . zip [1..]
    Vertex  -> concatLines              . (showLine <$>) . zip [1..]
    where
        pruneBoard board = (pruneLine (length board * 2) <$>) <$> board
        pruneLine l (c:cs) = c: drop l cs
        pruneLine _ [] = []

        showLine :: (Int, [Maybe Block]) -> [String]
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

instance Show GameSettings where show gameSettings = "Settings {" <> show gameSettings.getPlayer1
                                                    <>  " , "     <> show gameSettings.getTurnDuration
                                                    <>  " , "     <> show gameSettings.getBoardS0
                                                    <>  " }"
instance Show GameState where show gameState = "Game {" <> show gameState.getPlayer'sTurn
                                            <>  " , "   <> show gameState.getDeadline
                                            <>  " , "   <> show gameState.getBoard
                                            <>  " }"
instance Show GameInfo where show gameInfo = "GameInfo {" <> show gameInfo.getPlayers
                                            <>  " , "     <> show gameInfo.getTurnDuration'
                                            <>  " , "     <> show gameInfo.getGameState
                                            <>  " }"

-------------------------------------------------- | Utility Functions | ---------------------------------------------------

destructureBoard :: Board -> [Block]
destructureBoard (Board boardMap) = AssocMap.toList boardMap

-------------------------------------------------- | PlutusTx Instances | --------------------------------------------------

-- PlutusTx.ToData , PlutusTx.FromData , PlutusTx.UnsafeFromData
PlutusTx.makeIsDataIndexed ''Player         [('RedPlayer, 0),('BluePlayer, 1)]
PlutusTx.makeIsDataIndexed ''Hexagon        [('Empty, 0),('Red, 1),('Blue, 2)]
PlutusTx.makeIsDataIndexed ''Position       [('Position,0)]
instance PlutusTx.ToData Board              where toBuiltinData (Board a) = PlutusTx.toBuiltinData a
instance PlutusTx.FromData Board            where fromBuiltinData = (Board <$>) . PlutusTx.fromBuiltinData
instance PlutusTx.UnsafeFromData Board      where unsafeFromBuiltinData = Board . PlutusTx.unsafeFromBuiltinData
PlutusTx.makeIsDataIndexed ''Move           [('Move,0)]
PlutusTx.makeIsDataIndexed ''GameSettings   [('Settings,0)]
PlutusTx.makeIsDataIndexed ''GameState      [('Game,0)]
PlutusTx.makeIsDataIndexed ''GameInfo       [('GameInfo,0)]
PlutusTx.makeIsDataIndexed ''Initialization [('Add,0),('Withdraw,1)]
PlutusTx.makeIsDataIndexed ''RunGame        [('PlayTurn,0),('GameOver,1),('Draw,2),('TimeOut,3)]

-- PlutusTx.Eq instance
instance PlutusTx.Eq Player         where (==) = (==)
instance PlutusTx.Eq Hexagon        where (==) = (==)
instance PlutusTx.Eq Position       where (==) = (==)
instance PlutusTx.Eq Move           where (==) = (==)
instance PlutusTx.Eq Board          where (==) = (==)
instance PlutusTx.Eq Initialization where (==) = (==)
instance PlutusTx.Eq GameSettings   where (==) = (==)
instance PlutusTx.Eq GameState      where (==) = (==)
instance PlutusTx.Eq GameInfo       where (==) = (==)
instance PlutusTx.Eq RunGame        where (==) = (==)

-- PlutusTx.Show instance
instance PlutusTx.Show Player           where show = stringToBuiltinString . show
instance PlutusTx.Show Hexagon          where show = stringToBuiltinString . show
instance PlutusTx.Show Position         where show = stringToBuiltinString . show
instance PlutusTx.Show Move             where show = stringToBuiltinString . show
instance PlutusTx.Show Board            where show = stringToBuiltinString . show
instance PlutusTx.Show Initialization   where show = stringToBuiltinString . show
instance PlutusTx.Show GameSettings     where show = stringToBuiltinString . show
instance PlutusTx.Show GameState        where show = stringToBuiltinString . show
instance PlutusTx.Show GameInfo         where show = stringToBuiltinString . show
instance PlutusTx.Show RunGame          where show = stringToBuiltinString . show

----------------------------------------------------------------------------------------------------------------------------