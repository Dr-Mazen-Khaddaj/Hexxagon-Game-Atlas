module  DAppConfig  ( Config (..)
                    ) where

import GeniusYield.GYConfig ( GYCoreConfig )
import GeniusYield.Types    ( GYAddress, GYAssetClass, GYUTxOs )
import Data.Set             ( Set )

----------------------------------------------------------------------------------------------------------------------------

data Config = Config
    { getCoreConfig         :: GYCoreConfig
    , getWalletAddresses    :: [GYAddress]
    , getChangeAddress      :: GYAddress
    , getWalletUTxOs        :: GYUTxOs
    , getPlayerNFTs         :: Set GYAssetClass
    }

instance Show Config where
    show :: Config -> String
    show (Config coreConfig walletAddrs changeAddr walletUTxOs playerNFTs) =
        "-- Config --"      <> "\n" <>
        show coreConfig     <> "\n" <>
        show walletAddrs    <> "\n" <>
        show changeAddr     <> "\n" <>
        show walletUTxOs    <> "\n" <>
        show playerNFTs     <> "\n" <>
        "------------"

----------------------------------------------------------------------------------------------------------------------------