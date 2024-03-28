module Actions.JoinGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  PlutusTx.IsData             ( UnsafeFromData(unsafeFromBuiltinData) )
import  PlutusLedgerApi.V1.Value    ( AssetClass(..) )
import  PlutusLedgerApi.V2          ( POSIXTime )
import  DAppConfig                  ( Config (..) )
import  DataTypes                   ( Initialization (Add), Player (RedPlayer), GameInfo (GameInfo), GameState (Game), GameSettings (..) )
import  Instances                   ()
import  GYUtilities                 ( utxoHasAnyAsset )
import  IOUtilities                 ( chooseIndex )
import  Data.Set                    qualified as Set
import  Scripts                     qualified

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYScript 'PlutusV2 -> GYUTxO -> GYAssetClass -> GYAddress -> POSIXTime -> GYSlot -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton initialiseGameSC gameToStart identifierNFT runGameSCAddress currentTime currentSlot = pure
    $  mustHaveInput (GYTxIn gameToStartRef (GYTxInWitnessScript initialiseGameSCInScript gameSettingsGYDatum addPlayer))
    <> mustHaveOutput (GYTxOut runGameSCAddress totalBet (Just (gameInfo , GYTxOutUseInlineDatum)) Nothing)
    <> isInvalidBefore currentSlot
    where
        gameToStartRef = utxoRef gameToStart
        initialiseGameSCInScript = GYInScript $ Scripts.gyScriptToValidator initialiseGameSC
        gameSettingsGYDatum = case utxoOutDatum gameToStart of GYOutDatumInline d -> d ; _ -> error "Game To Start has no Inline Datum!"
        gameSettings = unsafeFromBuiltinData @GameSettings $ datumToPlutus' gameSettingsGYDatum
        addPlayer = redeemerFromPlutusData (Add player)
        player = RedPlayer nftSymbol nftName
        AssetClass (nftSymbol, nftName) = assetClassToPlutus identifierNFT
        totalBet = gameToStart.utxoValue <> gameToStart.utxoValue
        gameInfo = datumFromPlutusData $ GameInfo [gameSettings.getPlayer1, player] gameSettings.getTurnDuration gameState
        gameState = Game player (currentTime + gameSettings.getTurnDuration) gameSettings.getBoardS0

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: Config -> GYProviders -> IO GYTxBody
action (Config coreCfg walletAddrs changeAddr _ playerNFTs) providers = do
    initialiseGameSCScript  <- Scripts.initialiseGameSC
    initialiseGameSCUTxOs   <- query $ utxosAtAddress (Scripts.gyScriptToAddress initialiseGameSCScript) Nothing
    gameToStart             <- case filterUTxOs (utxoHasAnyAsset playerNFTs) initialiseGameSCUTxOs of
                                    (utxosSize -> 0) -> error "No Games available to Start!"
                                    xs -> selectUTxO xs
    runGameSCAddress        <- Scripts.gyScriptToAddress <$> Scripts.runGameSC
    identifierNFT           <- case playerNFTs of
                                    (Set.size -> 0) -> error "No Game NFTs found! Please Mint an NFT!"
                                    (Set.size -> 1) -> pure $ Set.elemAt 0 playerNFTs
                                    _               -> putStrLn "Multiple NFTs found!" >> selectNFT playerNFTs
    currentSlot             <- gyGetSlotOfCurrentBlock providers
    currentTime             <- timeToPlutus <$> query (slotToBeginTime currentSlot)
    runTx $ skeleton initialiseGameSCScript gameToStart identifierNFT runGameSCAddress currentTime currentSlot
    where
        networkID = cfgNetworkId coreCfg
        query :: GYTxQueryMonadNode a -> IO a
        query = runGYTxQueryMonadNode networkID providers
        runTx = runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing

selectUTxO :: GYUTxOs -> IO GYUTxO
selectUTxO utxos = (!!) (utxosToList utxos) <$> chooseIndex "UTxO" (utxosRefs utxos)

selectNFT :: Set.Set GYAssetClass -> IO GYAssetClass
selectNFT (Set.toList -> nfts) = (!!) nfts <$> chooseIndex "NFT" nfts

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |