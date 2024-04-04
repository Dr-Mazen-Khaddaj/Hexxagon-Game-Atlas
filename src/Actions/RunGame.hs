module Actions.RunGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  PlutusCore.Data             ( Data(..) )
import  PlutusTx.IsData             ( UnsafeFromData(unsafeFromBuiltinData) )
import  DAppConfig                  ( Config (..) )
import  DataTypes                   ( GameInfo (..), GameState (..), RunGame (..), Metadata (..), Player )
import  Instances                   ()
import  UtilityFxs                  ( makeMove, bytesFromHex )
import  GYUtilities                 hiding (availableToPlayer)
import  IOUtilities                 ( chooseIndex )
import  IOFxs                       ( playTurn )
import  MainFxs                     ( checkGameStatus )
import  Data.Maybe                  ( fromMaybe )
import  Scripts                     qualified
import  Data.Set                    qualified as Set
import  Data.List                   qualified as List
import  PlutusTx.AssocMap           qualified as AssocMap
import  Data.ByteString             qualified as BS

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYScript 'PlutusV2 -> GYUTxO -> GYTxOutRef -> RunGame -> Maybe Player -> GYSlot -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton runGameSC gameToRun authNFTRef runGame@(PlayTurn move) _ currentSlot = pure
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

skeleton runGameSC gameToEnd authNFTRef Draw (Just player) _ = pure
    $  mustHaveInput (GYTxIn gameToEndRef (GYTxInWitnessScript runGameSCInScript gameInfoGYDatum runGameGYRedeemer))
    <> mustHaveOutput (GYTxOut runGameSCAddress remainingAmount (Just (newGameInfo , GYTxOutUseInlineDatum)) Nothing)
    <> mustHaveInput (GYTxIn authNFTRef GYTxInWitnessKey)
    where
        gameToEndRef = utxoRef gameToEnd
        runGameSCInScript = GYInScript $ Scripts.gyScriptToValidator runGameSC
        gameInfoGYDatum = case utxoOutDatum gameToEnd of GYOutDatumInline d -> d ; _ -> error "Game To End has no Inline Datum!"
        runGameGYRedeemer = redeemerFromPlutusData Draw

        remainingAmount = divValue gameToEnd.utxoValue 2
        runGameSCAddress = Scripts.gyScriptToAddress runGameSC
        gameInfo = gameInfoFromUTxO gameToEnd
        turnDuration = gameInfo.getTurnDuration'
        remainingPlayers = List.delete player gameInfo.getPlayers
        newGameInfo = datumFromPlutusData $ GameInfo remainingPlayers turnDuration gameInfo.getGameState

skeleton runGameSC gameToEnd authNFTRef endGame _ currentSlot = pure
    $  mustHaveInput (GYTxIn gameToEndRef (GYTxInWitnessScript runGameSCInScript gameInfoGYDatum runGameGYRedeemer))
    <> mustHaveInput (GYTxIn authNFTRef GYTxInWitnessKey)
    <> isInvalidBefore currentSlot
    where
        gameToEndRef = utxoRef gameToEnd
        runGameSCInScript = GYInScript $ Scripts.gyScriptToValidator runGameSC
        gameInfoGYDatum = case utxoOutDatum gameToEnd of GYOutDatumInline d -> d ; _ -> error "Game To End has no Inline Datum!"
        runGameGYRedeemer = redeemerFromPlutusData endGame

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

action :: Config -> GYProviders -> IO (Either [String] GYTxBody)
action (Config coreCfg walletAddrs changeAddr walletUTxOs playerNFTs) providers = do
    runGameSCScript     <- Scripts.runGameSC
    runGameSCUTxOs      <- query $ utxosAtAddress (Scripts.gyScriptToAddress runGameSCScript) Nothing
    currentTime         <- timeToPlutus <$> getCurrentGYTime
    let availableToPlayer :: GYUTxO -> Bool
        availableToPlayer utxo = case fromUTxO @GameInfo utxo >>= checkGameStatus currentTime of
            Just TimeOut    -> case oppRegNFTFromUTxO utxo  of  Just regNFT -> Set.member regNFT playerNFTs
                                                                Nothing -> False
            Just Draw       -> case regNFTsFromUTxO utxo    of  Just regNFTs -> any (`Set.member` playerNFTs) regNFTs
                                                                Nothing -> False
            Nothing         -> case authNFTFromUTxO utxo    of  Just authNFT -> Set.member authNFT playerNFTs
                                                                Nothing -> False
            Just (GameOver winner) -> Set.member (playerToGYAssetClass winner) playerNFTs
            _ -> False
    case filterUTxOs availableToPlayer runGameSCUTxOs of
        (utxosSize -> 0) -> pure $ Left ["No Games available to Start!"]
        xs -> do
            gameToRun <- selectUTxO xs
            let gameInfo = gameInfoFromUTxO gameToRun
            playTurn gameInfo >>= \case
                Nothing -> pure $ Left ["Intentional Exit"]
                Just runGame -> Right <$> do
                    currentSlot <- gyGetSlotOfCurrentBlock providers
                    print runGame
                    case runGame of
                        Draw -> do
                            let registeredNFTs      = playerToGYAssetClass <$> gameInfo.getPlayers
                                authNFTUTxO         = head . utxosToList $ filterUTxOs (holdsRegNFT registeredNFTs) walletUTxOs
                                authNFTRef          = utxoRef authNFTUTxO
                                registeredPlayer    = head $ filter (\ p -> valueHasAssetClass (playerToGYAssetClass p) (utxoValue authNFTUTxO)) gameInfo.getPlayers
                            case gameInfo.getPlayers of
                                [_] -> runTx $ skeleton runGameSCScript gameToRun authNFTRef runGame Nothing currentSlot
                                _   -> runTx $ skeleton runGameSCScript gameToRun authNFTRef runGame (Just registeredPlayer) currentSlot
                        GameOver (playerToGYAssetClass -> winnerNFT) -> do
                            let authNFTRef = case utxosToList $ filterUTxOs (utxoHasAssetClass winnerNFT) walletUTxOs of
                                                (utxo:_) -> utxoRef utxo
                                                _        -> error "Only the winner is eligible to claim the reward!"
                                refNFT = case winnerNFT of
                                    GYToken symbol (GYTokenName name) ->
                                        let refNFTLabel = bytesFromHex "000643b0"
                                            refNFTName  = GYTokenName $ refNFTLabel <> BS.drop 4 name
                                        in  GYToken symbol refNFTName
                                    _ -> error "Invalid NFT!"
                            refNFTManagerSCScript <- Scripts.refNFTManagerSC
                            let refNFTManagerSCAddr = Scripts.gyScriptToAddress refNFTManagerSCScript
                            [refNFTUTxO] <- utxosToList <$> query (getUTxOsByNFT refNFTManagerSCAddr refNFT)
                            runTx $ mconcat <$> sequence    [ skeleton runGameSCScript gameToRun authNFTRef runGame Nothing currentSlot
                                                            , updateMetadataSkeleton refNFTManagerSCScript refNFTUTxO
                                                            ]
                        _ -> do
                            let identifierNFT   = playerToGYAssetClass $ getPlayer'sTurn gameInfo.getGameState
                                authNFTRef      = utxoRef . head . utxosToList $ filterUTxOs (utxoHasAssetClass identifierNFT) walletUTxOs
                            runTx $ skeleton runGameSCScript gameToRun authNFTRef runGame Nothing currentSlot
    where
        networkID = cfgNetworkId coreCfg
        query = runGYTxQueryMonadNode networkID providers
        runTx = runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing

selectUTxO :: GYUTxOs -> IO GYUTxO
selectUTxO utxos = (!!) (utxosToList utxos) <$> chooseIndex "Game" (utxosRefs utxos)

gameInfoFromUTxO :: GYUTxO -> GameInfo
gameInfoFromUTxO utxo = fromMaybe (error "Can't get GameInfo from UTxO!") (fromUTxO utxo)

holdsRegNFT :: [GYAssetClass] -> GYUTxO -> Bool
holdsRegNFT nfts utxo = any (`utxoHasAssetClass` utxo) nfts

divValue :: GYValue -> Integer -> GYValue
divValue v n = valueFromList [(,) a (div m n) | (a,m) <- valueToList v]

-- returns Just (GameOver winner) , Just Draw , Just TimeOut , or Nothing
_checkGameStatus :: GameInfo -> Maybe RunGame
_checkGameStatus = error "Undefined"

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |