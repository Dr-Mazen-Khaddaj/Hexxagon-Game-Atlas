module Actions.CancelGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  Data.Maybe      ( fromMaybe )
import  DAppConfig      ( Config (..) )
import  DataTypes       ( Initialization (Withdraw), GameSettings (getPlayer1) )
import  Instances       ()
import  GYUtilities     ( availableToPlayer, utxoHasAssetClass, playerToGYAssetClass, fromUTxO )
import  IOUtilities     ( chooseIndex )
import  Scripts         qualified

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYScript 'PlutusV2 -> GYUTxO -> GYTxOutRef -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton initialiseGameSC gameToCancel authNFTRef = pure
    $  mustHaveInput (GYTxIn gameToCancelRef (GYTxInWitnessScript initialiseGameSCInScript gameSettings withdraw))
    <> mustHaveInput (GYTxIn authNFTRef GYTxInWitnessKey)
    where
        gameToCancelRef = utxoRef gameToCancel
        initialiseGameSCInScript = GYInScript $ Scripts.gyScriptToValidator initialiseGameSC
        gameSettings = case utxoOutDatum gameToCancel of GYOutDatumInline d -> d ; _ -> error "Game To Cancel has no Inline Datum!"
        withdraw = redeemerFromPlutusData Withdraw

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: Config -> GYProviders -> IO (Either [String] GYTxBody)
action (Config coreCfg walletAddrs changeAddr walletUTxOs playerNFTs) providers = do
    initialiseGameSCScript  <- Scripts.initialiseGameSC
    initialiseGameSCUTxOs   <- query $ utxosAtAddress (Scripts.gyScriptToAddress initialiseGameSCScript) Nothing
    gameToCancel            <- case filterUTxOs (availableToPlayer playerNFTs) initialiseGameSCUTxOs of
                                (utxosSize -> 0) -> pure Nothing
                                xs -> Just <$> selectUTxO xs
    case gameToCancel of
        Just game -> do
            let gameSettings    = fromMaybe (error "Can't get GameSettings from UTxO!") (fromUTxO @GameSettings game)
                identifierNFT   = playerToGYAssetClass gameSettings.getPlayer1
                authNFTRef      = utxoRef . head . utxosToList $ filterUTxOs (utxoHasAssetClass identifierNFT) walletUTxOs
            Right <$> runTx (skeleton initialiseGameSCScript game authNFTRef)
        Nothing -> pure $ Left ["No Games available to Cancel!"]
    where
        networkID = cfgNetworkId coreCfg
        query = runGYTxQueryMonadNode networkID providers
        runTx = runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing

selectUTxO :: GYUTxOs -> IO GYUTxO
selectUTxO utxos = (!!) (utxosToList utxos) <$> chooseIndex "Game" (utxosRefs utxos)

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |