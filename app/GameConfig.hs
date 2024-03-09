module GameConfig (getGameConfig, Config (..)) where

import GeniusYield.GYConfig
import GeniusYield.Types
import System.Directory     (doesDirectoryExist, createDirectory)
import System.FilePath      (takeBaseName)
import Control.Monad        (unless)
import IOUtilities

--------------------------------------------------------------------------------------------------------------------------- |
data Config = Config
    { getCoreConfig :: GYCoreConfig
    , getSigningKey :: GYPaymentSigningKey
    , getAddress    :: GYAddress
    , getPlayerNFT  :: GYAssetClass
    , getLatestTxID :: Maybe GYTxId
    }

instance Show Config where
    show (Config coreConfig signingKey address gameNFT latestTxID) =
        "-- Config --"    <> "\n" <>
        show coreConfig   <> "\n" <>
        show signingKey   <> "\n" <>
        show address      <> "\n" <>
        show gameNFT      <> "\n" <>
        show latestTxID   <> "\n" <>
        "------------"
--------------------------------------------------------------------------------------------------------------------------- |

getGameConfig :: IO Config
getGameConfig = do
    coreCfg <- coreConfigIO "./Configurations/config.json"
    signingKey  <- checkForPaymentSigningKey >>= \case  True  -> getPaymentSigningKey
                                                        False -> makeNewPaymentSigningKey
    let walletAddr = addressFromPaymentKeyHash GYTestnetPreview . paymentKeyHash $ paymentVerificationKey signingKey
    let nft = GYLovelace -- This should be changed to a real NFT after the minting action is implemented.
    pure $ Config coreCfg signingKey walletAddr nft Nothing

--------------------------------------------------------------------------------------------------------------------------- |

checkForPaymentSigningKey :: IO Bool
checkForPaymentSigningKey = do
    let dir = "./keys/"
    dirExists <- doesDirectoryExist dir
    if dirExists then do
        n <- length <$> fetchFilesWithExtension dir ".SigningKey"
        if n > 0 then do
            putStrLn $ "You have " <> show n <> " Key(s) available."
            putStr "Do you want to create a new Key? "
            not <$> askYesNo
        else return False
    else return False

--------------------------------------------------------------------------------------------------------------------------- |

makeNewPaymentSigningKey :: IO GYPaymentSigningKey
makeNewPaymentSigningKey = do
    putStrLn " --- Generating New Payment Signing Key ---"
    putStr "Please enter a name for your new key : "
    name <- getLine'
    signingKey <- generatePaymentSigningKey
    writePSKToFile name signingKey
    pure signingKey

writePSKToFile :: String -> GYPaymentSigningKey -> IO ()
writePSKToFile name key = do
    let dir = "keys/"
    dirExists <- doesDirectoryExist dir
    unless dirExists $ createDirectory dir
    writePaymentSigningKey (dir <> name <> ".SigningKey") key

--------------------------------------------------------------------------------------------------------------------------- |

getPaymentSigningKey :: IO GYPaymentSigningKey
getPaymentSigningKey = do
    files <- fetchFilesWithExtension "./keys/" ".SigningKey"
    chosenFile <- (files !!) <$> chooseIndex "Key" (takeBaseName <$> files)
    readPaymentSigningKey chosenFile

--------------------------------------------------------------------------------------------------------------------------- |