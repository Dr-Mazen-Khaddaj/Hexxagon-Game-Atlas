module Actions.CreateGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  PlutusLedgerApi.V1.Value    ( AssetClass(..) )
import  PlutusLedgerApi.V2          ( POSIXTime(POSIXTime) )
import  IOUtilities                 ( chooseIndex )
import  DAppConfig                  ( Config (..) )
import  DataTypes                   ( Player(BluePlayer), GameSettings(Settings) )
import  Instances                   ()
import  Constants                   ( thousand, million, classicBoard_S5DC3 )
import  Data.Set                    qualified as Set
import  Scripts                     qualified

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYAssetClass -> GYAddress -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton identifierNFT initialiseGameSCAddress = pure
    $ mustHaveOutput (GYTxOut initialiseGameSCAddress betAmount (Just (gameSettings , GYTxOutUseInlineDatum)) Nothing)
    where
    -- betAmount is 100 ADA , turnDuration is 1 hour , and starting board is classicBoard_S5DC3
        betAmount       = valueSingleton GYLovelace (100*million)
        gameSettings    = datumFromPlutusData $ Settings player turnDuration boardS0
        player          = BluePlayer nftSymbol nftName
        turnDuration    = POSIXTime 60*60*thousand
        boardS0         = classicBoard_S5DC3
        AssetClass (nftSymbol, nftName) = assetClassToPlutus identifierNFT

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: Config -> GYProviders -> IO (Either [String] GYTxBody)
action (Config coreCfg walletAddrs changeAddr _ playerNFTs) providers = do
    initialiseGameSCAddress <- Scripts.gyScriptToAddress <$> Scripts.initialiseGameSC
    identifierNFT <- case playerNFTs of (Set.size -> 0) -> pure Nothing
                                        (Set.size -> 1) -> pure . Just $ Set.elemAt 0 playerNFTs
                                        _ -> putStrLn "Multiple NFTs found!" >> Just <$> selectNFT playerNFTs
    case identifierNFT of
        Just nft -> Right <$> runTx (skeleton nft initialiseGameSCAddress)
        Nothing -> pure $ Left ["No Game NFTs found! Please Mint an NFT!"]
    where
        networkID = cfgNetworkId coreCfg
        runTx = runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing

selectNFT :: Set.Set GYAssetClass -> IO GYAssetClass
selectNFT (Set.toList -> nfts) = (!!) nfts <$> chooseIndex "NFT" nfts

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |