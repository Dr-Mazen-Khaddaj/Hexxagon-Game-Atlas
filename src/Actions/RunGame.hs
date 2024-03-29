{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Actions.RunGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  PlutusTx.IsData             ( UnsafeFromData(unsafeFromBuiltinData) )
import  DAppConfig                  ( Config (..) )
import  DataTypes                   ( GameInfo (..), GameState (..), RunGame (..) )
import  Instances                   ()
import  UtilityFxs                  ( makeMove )
import  GYUtilities                 ( utxoGameHasAnyAsset )
import  IOUtilities                 ( chooseIndex )
import  IOFxs                       ( playTurn )
import  Scripts                     qualified
import  Data.List                   qualified as List

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYScript 'PlutusV2 -> GYUTxO -> RunGame -> GYSlot -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton runGameSC gameToRun runGame@(PlayTurn move) currentSlot = pure
    $  mustHaveInput (GYTxIn gameToRunRef (GYTxInWitnessScript runGameSCInScript gameInfoGYDatum runGameGYDatum))
    <> mustHaveOutput (GYTxOut runGameSCAddress totalBet (Just (newGameInfo , GYTxOutUseInlineDatum)) Nothing)
    <> isInvalidBefore currentSlot
    where
        gameToRunRef = utxoRef gameToRun
        runGameSCInScript = GYInScript $ Scripts.gyScriptToValidator runGameSC
        gameInfoGYDatum = case utxoOutDatum gameToRun of GYOutDatumInline d -> d ; _ -> error "Game To Run has no Inline Datum!"
        runGameGYDatum = redeemerFromPlutusData runGame

        totalBet = gameToRun.utxoValue
        runGameSCAddress = Scripts.gyScriptToAddress runGameSC
        gameInfo = gameInfoFromUTxO gameToRun
        players = gameInfo.getPlayers
        turnDuration = gameInfo.getTurnDuration'
        Game player deadline board = gameInfo.getGameState
        nextPlayer = case List.find (/= player) players of Just p -> p ; _ -> error "Can't find next Player!"
        newGameState = Game nextPlayer (deadline + turnDuration) (makeMove move board)
        newGameInfo = datumFromPlutusData $ GameInfo players turnDuration newGameState

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: Config -> GYProviders -> IO GYTxBody
action (Config coreCfg walletAddrs changeAddr _ playerNFTs) providers = do
    runGameSCScript         <- Scripts.runGameSC
    runGameSCUTxOs          <- query $ utxosAtAddress (Scripts.gyScriptToAddress runGameSCScript) Nothing
    gameToRun               <- case filterUTxOs (utxoGameHasAnyAsset playerNFTs) runGameSCUTxOs of
                                    (utxosSize -> 0) -> error "No Games available to Start!"
                                    xs -> selectUTxO xs
    runGame                 <- playTurn $ gameInfoFromUTxO gameToRun
    currentSlot             <- gyGetSlotOfCurrentBlock providers
    runTx $ skeleton runGameSCScript gameToRun runGame currentSlot
    where
        networkID = cfgNetworkId coreCfg
        query = runGYTxQueryMonadNode networkID providers
        runTx = runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing

selectUTxO :: GYUTxOs -> IO GYUTxO
selectUTxO utxos = (!!) (utxosToList utxos) <$> chooseIndex "Game" (utxosRefs utxos)

gameInfoFromUTxO :: GYUTxO -> GameInfo
gameInfoFromUTxO utxo = case utxoOutDatum utxo of
                            GYOutDatumInline d -> unsafeFromBuiltinData $ datumToPlutus' d
                            _ -> error "Game To Run has no Inline Datum!"

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |