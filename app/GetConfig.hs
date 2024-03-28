module GetConfig (getLocalConfig) where

import GeniusYield.Types
import DAppConfig           (LocalConfig (..), Config (Config))
import System.Directory     (doesDirectoryExist, createDirectory)
import IOUtilities          (fetchFilesWithExtension, askYesNo, getLine', chooseIndex, ToColor (..))
import GeniusYield.GYConfig (coreConfigIO, withCfgProviders, cfgNetworkId)
import Control.Monad        (unless)
import System.FilePath      (takeBaseName)
import UtilityFxs (bytesFromHex)
import qualified Data.Set as Set
import GeniusYield.TxBuilder (runGYTxQueryMonadNode, utxosAtAddresses)
import GYUtilities (getAssets)

--------------------------------------------------------------------------------------------------------------------------

getLocalConfig :: IO LocalConfig
getLocalConfig = do
    coreCfg <- coreConfigIO "./Configurations/config.json"
    signingKey  <- checkForPaymentSigningKey >>= \case  True  -> getPaymentSigningKey
                                                        False -> makeNewPaymentSigningKey
    let walletAddr = addressFromPaymentKeyHash GYTestnetPreview . paymentKeyHash $ paymentVerificationKey signingKey
        networkID = cfgNetworkId coreCfg
    walletUTxOs <- withCfgProviders coreCfg (toIGreen "Get Wallet UTxOs") $ \ providers ->
        runGYTxQueryMonadNode networkID providers $ utxosAtAddresses [walletAddr]
    let playerNFTs = Set.filter belongToGame $ getAssets walletUTxOs
    pure $ LocalConfig (Config coreCfg [walletAddr] walletAddr walletUTxOs playerNFTs) signingKey

belongToGame :: GYAssetClass -> Bool
belongToGame GYLovelace = False
belongToGame (GYToken _ (GYTokenName name)) = name == bytesFromHex "000de140" <> "HEXXAGON"

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

getPaymentSigningKey :: IO GYPaymentSigningKey
getPaymentSigningKey = do
    files <- fetchFilesWithExtension "./keys/" ".SigningKey"
    chosenFile <- (files !!) <$> chooseIndex "Key" (takeBaseName <$> files)
    readPaymentSigningKey chosenFile

--------------------------------------------------------------------------------------------------------------------------