module  DAppConfig  ( Config (..)
                    , LocalConfig (..)
                    ) where

import GeniusYield.GYConfig ( GYCoreConfig )
import GeniusYield.Types    ( GYAddress, GYPaymentSigningKey, GYTxId, GYAssetClass )

----------------------------------------------------------------------------------------------------------------------------

data Config = Config
    { getCoreConfig         :: GYCoreConfig
    , getWalletAddresses    :: [GYAddress]
    , getChangeAddresses    :: GYAddress
    , getPlayerNFT          :: GYAssetClass
    , getLatestTxID         :: Maybe GYTxId
    }

instance Show Config where
    show :: Config -> String
    show (Config coreConfig walletAddrs changeAddr gameNFT latestTxID) =
        "-- Config --"    <> "\n" <>
        show coreConfig   <> "\n" <>
        show walletAddrs  <> "\n" <>
        show changeAddr   <> "\n" <>
        show gameNFT      <> "\n" <>
        show latestTxID   <> "\n" <>
        "------------"

data LocalConfig = LocalConfig
    { getConfig     :: Config
    , getSigningKey :: GYPaymentSigningKey
    } deriving Show

----------------------------------------------------------------------------------------------------------------------------