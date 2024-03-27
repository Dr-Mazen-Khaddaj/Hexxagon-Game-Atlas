module GetConfig (getConfig) where

import DAppConfig (Config (..))
import GeniusYield.GYConfig (coreConfigIO)
import GetAddresses (fetchWalletAddresses)
import GeniusYield.Types (GYAssetClass(..))

getConfig :: IO Config
getConfig = do
    coreCfg <- coreConfigIO "./Configurations/config.json"
    (walletAddresses, changeAddress) <- fetchWalletAddresses
    let nft = GYLovelace -- This should be changed to a real NFT after the minting action is implemented.
    pure $ Config coreCfg walletAddresses changeAddress nft Nothing
