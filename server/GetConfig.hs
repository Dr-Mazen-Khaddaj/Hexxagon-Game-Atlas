module GetConfig (getConfig) where

import DAppConfig (Config (..))
import GeniusYield.GYConfig (coreConfigIO, cfgNetworkId, withCfgProviders)
import GetAddresses (fetchWalletAddresses)
import GeniusYield.Types (GYAssetClass (..), GYTokenName (GYTokenName))
import GeniusYield.TxBuilder (GYTxQueryMonad(..), runGYTxQueryMonadNode)
import GYUtilities (getAssets)
import Data.Set qualified as Set
import UtilityFxs (bytesFromHex)
import IOUtilities (toIGreen)

getConfig :: IO Config
getConfig = do
    (walletAddresses, changeAddress) <- fetchWalletAddresses
    coreCfg <- coreConfigIO "./Configurations/config.json"
    let networkID = cfgNetworkId coreCfg
    walletUTxOs <- withCfgProviders coreCfg (toIGreen "Get Wallet UTxOs") $ \ providers ->
        runGYTxQueryMonadNode networkID providers $ utxosAtAddresses walletAddresses
    let playerNFTs = Set.filter belongToGame $ getAssets walletUTxOs
    pure $ Config coreCfg walletAddresses changeAddress walletUTxOs playerNFTs

belongToGame :: GYAssetClass -> Bool
belongToGame GYLovelace = False
belongToGame (GYToken _ (GYTokenName name)) = name == bytesFromHex "000de140" <> "HEXXAGON"