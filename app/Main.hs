module Main (main) where

import Control.Monad.State      ( StateT (runStateT), get, liftIO, gets )
import GeniusYield.GYConfig     ( withCfgProviders )
import GeniusYield.Types        ( gySubmitTx, signGYTxBody, GYPaymentSigningKey, GYTxBody, GYProviders )
import Scripts                  ( gyScriptToAddress, initialiseGameSC, runGameSC, refNFTManagerSC )
import IOUtilities              ( printTxID, ToColor (..), chooseIndex )
import DAppConfig               ( Config(..), LocalConfig (..) )
import GetConfig                ( getLocalConfig )
import Actions.CreateGame       qualified
import Actions.CancelGame       qualified
import Actions.MintPlayerNFT    qualified

main :: IO ()
main = do
    initialiseGameSC        >>= print
    runGameSC               >>= print
    refNFTManagerSC         >>= print
    initialiseGameSC        >>= print . gyScriptToAddress
    runGameSC               >>= print . gyScriptToAddress
    refNFTManagerSC         >>= print . gyScriptToAddress

    welcomeScreen
    LocalConfig config walletSkey <- getLocalConfig
    print config
    _ <- runStateT (runDApp walletSkey) config
    pure ()

runDApp :: GYPaymentSigningKey -> StateT Config IO ()
runDApp walletSkey = do
    coreCfg <- gets getCoreConfig
    i <- liftIO $ chooseIndex @String "Action" ["Create Game", "Cancel Game" , "Mint Player NFT"]
    txBody <- case i of
        0 -> buildTxBody "CreateGame"       Actions.CreateGame.action
        1 -> buildTxBody "CancelGame"       Actions.CancelGame.action
        2 -> buildTxBody "MintPlayerNFT"    Actions.MintPlayerNFT.action
        _ -> error "Action Not available!"
    liftIO $ withCfgProviders coreCfg (toIGreen "SubmitTx") $ signAndSubmitTx txBody walletSkey

buildTxBody :: String -> (Config -> GYProviders -> IO GYTxBody) -> StateT Config IO GYTxBody
buildTxBody namespace action = do
    config@(Config coreCfg _ _ _ _) <- get
    liftIO $ withCfgProviders coreCfg (toIGreen namespace) $ action config

signAndSubmitTx :: GYTxBody -> GYPaymentSigningKey -> GYProviders -> IO ()
signAndSubmitTx txBody walletSkey providers = gySubmitTx providers (signGYTxBody txBody [walletSkey]) >>= printTxID

welcomeScreen :: IO ()
welcomeScreen = do
    putStrLn $ toICyan "\n\t\t--------------------------------------------------------"
    putStrLn $ toICyan   "\t\t--------------- Welcome To Hexxagon Game ---------------"
    putStrLn $ toICyan   "\t\t--------------------------------------------------------\n"
