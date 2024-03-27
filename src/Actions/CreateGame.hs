module Actions.CreateGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  PlutusLedgerApi.V1.Value    ( AssetClass(..) )
import  PlutusLedgerApi.V2          ( POSIXTime(POSIXTime) )
import  DataTypes                   ( Player(BluePlayer), GameSettings(Settings) )
import  Instances                   ()
import  Constants                   ( thousand, classicBoard_S9DC3, million )
import  Scripts                     qualified
import  DAppConfig                  (Config (..))

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYAssetClass -> GYAddress -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton identifierNFT initialiseGameSCAddress = pure
    $ mustHaveOutput (GYTxOut initialiseGameSCAddress betAmount (Just (gameSettings , GYTxOutUseInlineDatum)) Nothing)
    where
    -- betAmount is 100 ADA , turnDuration is 1 hour , and starting board is classicBoard_S9DC3
        betAmount       = valueSingleton GYLovelace (100*million)
        gameSettings    = datumFromPlutusData $ Settings player turnDuration boardS0
        player          = BluePlayer nftSymbol nftName
        turnDuration    = POSIXTime 60*60*thousand
        boardS0         = classicBoard_S9DC3
        AssetClass (nftSymbol, nftName) = assetClassToPlutus identifierNFT

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: Config -> GYProviders -> IO GYTxBody
action (Config coreCfg walletAddrs changeAddr identifierNFT _) providers = do
    initialiseGameSCAddress <- Scripts.gyScriptToAddress <$> Scripts.initialiseGameSC
    runTx $ skeleton identifierNFT initialiseGameSCAddress
    where
        networkID = cfgNetworkId coreCfg
        runTx = runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |