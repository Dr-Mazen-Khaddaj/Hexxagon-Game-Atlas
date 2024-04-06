module Actions.CreateGame (action) where

import  GeniusYield.Types
import  GeniusYield.TxBuilder
import  GeniusYield.GYConfig
import  PlutusLedgerApi.V1.Value    ( AssetClass(..) )
import  PlutusLedgerApi.V2          ( POSIXTime(POSIXTime) )
import  IOFxs                       ( createCustomBoard )
import  IOUtilities                 ( chooseIndex, getInt )
import  DAppConfig                  ( Config (..) )
import  DataTypes                   ( Player(..), GameSettings(..), Board )
import  Instances                   ()
import  Constants                   ( thousand, million, classicBoard_S5DC3 )
import  Data.Set                    qualified as Set
import  Scripts                     qualified

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYAssetClass -> GYAddress -> (GYValue,POSIXTime,Board) -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton identifierNFT initialiseGameSCAddress (betAmount,turnDuration,boardS0) = pure
    $ mustHaveOutput (GYTxOut initialiseGameSCAddress betAmount (Just (gameSettings , GYTxOutUseInlineDatum)) Nothing)
    where
        gameSettings    = datumFromPlutusData $ Settings player turnDuration boardS0
        player          = BluePlayer nftSymbol nftName
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
        Just nft -> getGameConfig >>= (Right <$>) . runTx . skeleton nft initialiseGameSCAddress
        Nothing -> pure $ Left ["No Game NFTs found! Please Mint an NFT!"]
    where
        networkID = cfgNetworkId coreCfg
        runTx s = do
            putStrLn "Building transaction ..."
            runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing s

selectNFT :: Set.Set GYAssetClass -> IO GYAssetClass
selectNFT (Set.toList -> nfts) = (!!) nfts <$> chooseIndex "NFT" nfts

getGameConfig :: IO (GYValue,POSIXTime,Board)
getGameConfig = do
    boardS0 <- (\case Just b -> b ; Nothing -> classicBoard_S5DC3) <$> createCustomBoard
    putStr "Enter bet amount (ADA) : "
    betAmount <- valueSingleton GYLovelace . (*million) <$> getInt [1..million]
    putStr "Enter turn duration (minutes) *range(5 - 3000) : "
    turnDuration <- POSIXTime . (*60) . (*thousand) <$> getInt [5..3000]
    pure (betAmount,turnDuration,boardS0)

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |