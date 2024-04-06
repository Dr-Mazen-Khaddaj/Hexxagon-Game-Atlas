module Actions.JoinGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  PlutusTx.IsData             ( UnsafeFromData(unsafeFromBuiltinData) )
import  PlutusLedgerApi.V1.Value    ( AssetClass(..) )
import  PlutusLedgerApi.V2          ( POSIXTime )
import  Data.Maybe                  ( isJust, fromJust )
import  DAppConfig                  ( Config (..) )
import  DataTypes                   ( Initialization (Add), Player (RedPlayer), GameInfo (GameInfo), GameState (Game), GameSettings (..) )
import  Instances                   ()
import  GYUtilities                 ( fromUTxO, playerToGYAssetClass )
import  IOUtilities                 ( chooseIndex )
import  Data.Set                    qualified as Set
import  Scripts                     qualified

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYScript 'PlutusV2 -> GYUTxO -> GYAssetClass -> GYAddress -> POSIXTime -> GYSlot -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton initialiseGameSC gameToJoin identifierNFT runGameSCAddress currentTime currentSlot = pure
    $  mustHaveInput (GYTxIn gameToStartRef (GYTxInWitnessScript initialiseGameSCInScript gameSettingsGYDatum addPlayer))
    <> mustHaveOutput (GYTxOut runGameSCAddress totalBet (Just (gameInfo , GYTxOutUseInlineDatum)) Nothing)
    <> isInvalidBefore currentSlot
    where
        gameToStartRef = utxoRef gameToJoin
        initialiseGameSCInScript = GYInScript $ Scripts.gyScriptToValidator initialiseGameSC
        gameSettingsGYDatum = case utxoOutDatum gameToJoin of GYOutDatumInline d -> d ; _ -> error "Game To Join has no Inline Datum!"
        gameSettings = unsafeFromBuiltinData @GameSettings $ datumToPlutus' gameSettingsGYDatum
        addPlayer = redeemerFromPlutusData (Add player)
        player = RedPlayer nftSymbol nftName
        AssetClass (nftSymbol, nftName) = assetClassToPlutus identifierNFT
        totalBet = gameToJoin.utxoValue <> gameToJoin.utxoValue
        gameInfo = datumFromPlutusData $ GameInfo [gameSettings.getPlayer1, player] gameSettings.getTurnDuration gameState
        gameState = Game player (currentTime + gameSettings.getTurnDuration) gameSettings.getBoardS0

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: Config -> GYProviders -> IO (Either [String] GYTxBody)
action (Config coreCfg walletAddrs changeAddr _ playerNFTs) providers = do
    initialiseGameSCScript  <- Scripts.initialiseGameSC
    initialiseGameSCUTxOs   <- query $ utxosAtAddress (Scripts.gyScriptToAddress initialiseGameSCScript) Nothing
    gameToJoin              <- case filterUTxOs validUTxO initialiseGameSCUTxOs of
                                    (utxosSize -> 0) -> pure Nothing
                                    xs -> Just <$> selectUTxO xs
    runGameSCAddress        <- Scripts.gyScriptToAddress <$> Scripts.runGameSC
    currentSlot             <- gyGetSlotOfCurrentBlock providers
    currentTime             <- timeToPlutus <$> query (slotToBeginTime currentSlot)
    case gameToJoin of
        Nothing -> pure $ Left ["No Games available to Join!"]
        Just game -> do
            let regNFT = playerToGYAssetClass . getPlayer1 . fromJust $ fromUTxO game
                availableNFTs = Set.filter (/= regNFT) playerNFTs
            identifierNFT <- case availableNFTs of
                                (Set.size -> 0) -> pure Nothing
                                (Set.size -> 1) -> pure . Just $ Set.elemAt 0 availableNFTs
                                _               -> putStrLn "Multiple NFTs found!" >> Just <$> selectNFT availableNFTs
            case identifierNFT of
                Nothing -> pure $ Left ["No Game NFTs found! Please Mint an NFT!"]
                Just nft -> Right <$> runTx (skeleton initialiseGameSCScript game nft runGameSCAddress currentTime currentSlot)
    where
        networkID = cfgNetworkId coreCfg
        query :: GYTxQueryMonadNode a -> IO a
        query = runGYTxQueryMonadNode networkID providers
        runTx s = do
            putStrLn "Building transaction ..."
            runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing s

selectUTxO :: GYUTxOs -> IO GYUTxO
selectUTxO utxos = (!!) (utxosToList utxos) <$> chooseIndex "Game" (utxosRefs utxos)

selectNFT :: Set.Set GYAssetClass -> IO GYAssetClass
selectNFT (Set.toList -> nfts) = (!!) nfts <$> chooseIndex "NFT" nfts

validUTxO :: GYUTxO -> Bool
validUTxO = isJust . fromUTxO @GameSettings

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |