{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Actions.RunGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  PlutusCore.Data             (Data(..))
import  PlutusTx.IsData             ( UnsafeFromData(unsafeFromBuiltinData) )
import  DAppConfig                  ( Config (..) )
import  DataTypes                   ( GameInfo (..), GameState (..), RunGame (..), Metadata (..) )
import  Instances                   ()
import  UtilityFxs                  ( makeMove, bytesFromHex )
import  GYUtilities                 ( utxoGameHasAnyAsset, playerToGYAssetClass, getUTxOByNFT, utxoHasAssetClass )
import  IOUtilities                 ( chooseIndex )
import  IOFxs                       ( playTurn )
import  Scripts                     qualified
import  Data.List                   qualified as List
import  PlutusTx.AssocMap           qualified as AssocMap
import  Data.ByteString             qualified as BS

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYScript 'PlutusV2 -> GYUTxO -> GYTxOutRef -> RunGame -> GYSlot -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton runGameSC gameToRun authNFTRef runGame@(PlayTurn move) currentSlot = pure
    $  mustHaveInput (GYTxIn gameToRunRef (GYTxInWitnessScript runGameSCInScript gameInfoGYDatum runGameGYRedeemer))
    <> mustHaveOutput (GYTxOut runGameSCAddress totalBet (Just (newGameInfo , GYTxOutUseInlineDatum)) Nothing)
    <> mustHaveInput (GYTxIn authNFTRef GYTxInWitnessKey)
    <> isInvalidBefore currentSlot
    where
        gameToRunRef = utxoRef gameToRun
        runGameSCInScript = GYInScript $ Scripts.gyScriptToValidator runGameSC
        gameInfoGYDatum = case utxoOutDatum gameToRun of GYOutDatumInline d -> d ; _ -> error "Game To Run has no Inline Datum!"
        runGameGYRedeemer = redeemerFromPlutusData runGame

        totalBet = gameToRun.utxoValue
        runGameSCAddress = Scripts.gyScriptToAddress runGameSC
        gameInfo = gameInfoFromUTxO gameToRun
        players = gameInfo.getPlayers
        turnDuration = gameInfo.getTurnDuration'
        Game player deadline board = gameInfo.getGameState
        nextPlayer = case List.find (/= player) players of Just p -> p ; _ -> error "Can't find next Player!"
        newGameState = Game nextPlayer (deadline + turnDuration) (makeMove move board)
        newGameInfo = datumFromPlutusData $ GameInfo players turnDuration newGameState

skeleton runGameSC gameToEnd authNFTRef TimeOut currentSlot = pure
    $  mustHaveInput (GYTxIn gameToEndRef (GYTxInWitnessScript runGameSCInScript gameInfoGYDatum runGameGYRedeemer))
    <> mustHaveInput (GYTxIn authNFTRef GYTxInWitnessKey)
    <> isInvalidBefore currentSlot
    where
        gameToEndRef = utxoRef gameToEnd
        runGameSCInScript = GYInScript $ Scripts.gyScriptToValidator runGameSC
        gameInfoGYDatum = case utxoOutDatum gameToEnd of GYOutDatumInline d -> d ; _ -> error "Game To End has no Inline Datum!"
        runGameGYRedeemer = redeemerFromPlutusData TimeOut

skeleton runGameSC gameToEnd authNFTRef gameOver@(GameOver _) currentSlot = pure
    $  mustHaveInput (GYTxIn gameToEndRef (GYTxInWitnessScript runGameSCInScript gameInfoGYDatum runGameGYRedeemer))
    <> mustHaveInput (GYTxIn authNFTRef GYTxInWitnessKey)
    <> isInvalidBefore currentSlot
    where
        gameToEndRef = utxoRef gameToEnd
        runGameSCInScript = GYInScript $ Scripts.gyScriptToValidator runGameSC
        gameInfoGYDatum = case utxoOutDatum gameToEnd of GYOutDatumInline d -> d ; _ -> error "Game To End has no Inline Datum!"
        runGameGYRedeemer = redeemerFromPlutusData gameOver

updateMetadataSkeleton :: GYScript 'PlutusV2 -> GYUTxO -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
updateMetadataSkeleton refNFTManagerSC refNFTUTxO = pure
    $  mustHaveInput (GYTxIn refNFTUTxO.utxoRef (GYTxInWitnessScript refNFTManagerSCInScript metadataGYDatum unitRedeemer))
    <> mustHaveOutput (GYTxOut refNFTManagerSCAddress refNFTUTxO.utxoValue (Just (newMetadata , GYTxOutUseInlineDatum)) Nothing)
    where
        refNFTManagerSCInScript = GYInScript $ Scripts.gyScriptToValidator refNFTManagerSC
        refNFTManagerSCAddress = Scripts.gyScriptToAddress refNFTManagerSC
        metadataGYDatum = case refNFTUTxO.utxoOutDatum of GYOutDatumInline d -> d ; _ -> error "RefNFT UTxO has no Inline Datum!"
        metadata = unsafeFromBuiltinData @Metadata $ datumToPlutus' metadataGYDatum
        newScore = case AssocMap.lookup "score" metadata.getMetadata of Just (I n) -> I (n+1) ; _ -> error "Can't find score in Metadata!"
        newMetadata = datumFromPlutusData $ Metadata (AssocMap.insert "score" newScore metadata.getMetadata) metadata.getVersionNum metadata.getExtraData

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: Config -> GYProviders -> IO GYTxBody
action (Config coreCfg walletAddrs changeAddr walletUTxOs playerNFTs) providers = do
    runGameSCScript     <- Scripts.runGameSC
    runGameSCUTxOs      <- query $ utxosAtAddress (Scripts.gyScriptToAddress runGameSCScript) Nothing
    gameToRun           <- case filterUTxOs (utxoGameHasAnyAsset playerNFTs) runGameSCUTxOs of
                                (utxosSize -> 0)    -> error "No Games available to Start!"
                                xs                  -> selectUTxO xs
    let gameInfo        = gameInfoFromUTxO gameToRun
    runGame             <- playTurn gameInfo
    currentSlot         <- gyGetSlotOfCurrentBlock providers
    print runGame
    case runGame of
        GameOver winner -> do
            let winnerNFT               = playerToGYAssetClass winner
                authNFTRef              = case utxosToList $ filterUTxOs (utxoHasAssetClass winnerNFT) walletUTxOs of
                                            (utxo:_) -> utxoRef utxo
                                            _        -> error "Only the winner is eligible to claim the reward!"
            refNFTManagerSCScript       <- Scripts.refNFTManagerSC
            let refNFTManagerSCAddr     = Scripts.gyScriptToAddress refNFTManagerSCScript
                GYToken refNFT_Symbol (GYTokenName name) = playerToGYAssetClass winner
                refNFTLabel             = bytesFromHex "000643b0"
                refNFTName              = GYTokenName $ refNFTLabel <> BS.drop 4 name
                refNFT                  = GYToken refNFT_Symbol refNFTName
            [refNFTUTxO]                <- utxosToList <$> query (getUTxOByNFT refNFTManagerSCAddr refNFT)
            runTx $ mconcat <$> sequence    [ skeleton runGameSCScript gameToRun authNFTRef runGame currentSlot
                                            , updateMetadataSkeleton refNFTManagerSCScript refNFTUTxO
                                            ]
        _ -> do
            let identifierNFT   = playerToGYAssetClass $ getPlayer'sTurn gameInfo.getGameState
                authNFTRef      = utxoRef . head . utxosToList $ filterUTxOs (utxoHasAssetClass identifierNFT) walletUTxOs
            runTx $ skeleton runGameSCScript gameToRun authNFTRef runGame currentSlot
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