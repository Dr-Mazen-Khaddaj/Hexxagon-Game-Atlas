module Actions.CancelGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  Data.Maybe      ( fromMaybe)
import  DataTypes       ( Initialization (Withdraw), GameSettings (getPlayer1) )
import  Instances       ()
import  GYUtilities     ( getAssets, utxoHasAnyAsset, utxoHasAssetClass, gameSettingsFromUTxO, playerToGYAssetClass )
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

action :: GYCoreConfig -> GYPaymentSigningKey -> GYAddress -> GYProviders -> IO GYTxId
action    coreCfg         walletSkey             walletAddr     providers  = do
    initialiseGameSCScript  <- Scripts.initialiseGameSC
    initialiseGameSCUTxOs   <- query $ utxosAtAddress (Scripts.gyScriptToAddress initialiseGameSCScript) Nothing
    walletUTxOs             <- query $ utxosAtAddress walletAddr Nothing
    gameToCancel            <- selectUTxO $ filterUTxOs (utxoHasAnyAsset $ getAssets walletUTxOs) initialiseGameSCUTxOs
    let identifierNFT = playerToGYAssetClass . getPlayer1 $ fromMaybe (error "Can't get GameSettings!") (gameSettingsFromUTxO gameToCancel)
        authNFTRef = utxoRef . head . utxosToList $ filterUTxOs (utxoHasAssetClass identifierNFT) walletUTxOs
    txBody <- runGYTxMonadNode networkID providers [walletAddr] walletAddr Nothing $ skeleton initialiseGameSCScript gameToCancel authNFTRef
    gySubmitTx providers $ signGYTxBody txBody [walletSkey]
    where
        networkID = cfgNetworkId coreCfg
        query = runGYTxQueryMonadNode networkID providers

selectUTxO :: GYUTxOs -> IO GYUTxO
selectUTxO utxos = (!!) (utxosToList utxos) <$> chooseIndex "UTxO" (utxosRefs utxos)

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |