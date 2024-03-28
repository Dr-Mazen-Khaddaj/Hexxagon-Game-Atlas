module Main where

import Control.Concurrent
import Control.Concurrent.Async (async)
import RunServer (runServer)
import DAppConfig (Config(..))
import GetConfig (getConfig)
import Control.Monad.Trans.State (StateT, runStateT, gets, get)
import qualified Actions.CreateGame
import qualified Actions.CancelGame
import qualified Actions.MintPlayerNFT
import GeniusYield.Types (GYProviders (..), GYTxBody, GYAwaitTxParameters (GYAwaitTxParameters))
import Control.Monad.IO.Class (liftIO)
import IOUtilities (chooseIndex, printTxID, ToColor (..), printMsg, UnquotedString (Unquoted))
import GeniusYield.GYConfig (withCfgProviders)
import SignTransaction (signTransaction)

main :: IO ()
main = do
    welcomeScreen
    usedAddrsVar        <- newEmptyMVar
    changeAddrVar       <- newEmptyMVar
    unsignedTxVar       <- newEmptyMVar
    signedWitnessVar    <- newEmptyMVar
    _serverThread       <- async $ runServer usedAddrsVar changeAddrVar unsignedTxVar signedWitnessVar
    config              <- getConfig
    printRetrievedAddresses config
    _ <- runStateT runDApp config
    pure ()

printRetrievedAddresses :: Config -> IO ()
printRetrievedAddresses (Config _ walletAddresses changeAddress _ _) = do
    putStrLn "Retrieved wallet addresses: " >> mapM_ print walletAddresses
    putStrLn "Change address: " >> print changeAddress

runDApp :: StateT Config IO ()
runDApp = do
    coreCfg <- gets getCoreConfig
    i <- liftIO $ chooseIndex "Action" $ Unquoted <$> ["Create Game", "Cancel Game" , "Mint Player NFT"]
    txBody <- case i of
        0 -> buildTxBody "CreateGame"       Actions.CreateGame.action
        1 -> buildTxBody "CancelGame"       Actions.CancelGame.action
        2 -> buildTxBody "MintPlayerNFT"    Actions.MintPlayerNFT.action
        _ -> error "Action Not available!"
    liftIO $ withCfgProviders coreCfg (toIGreen "SubmitTx") $ signAndSubmitTx txBody

buildTxBody :: String -> (Config -> GYProviders -> IO GYTxBody) -> StateT Config IO GYTxBody
buildTxBody namespace action = do
    config@(Config coreCfg _ _ _ _) <- get
    liftIO $ withCfgProviders coreCfg (toIGreen namespace) $ action config

signAndSubmitTx :: GYTxBody -> GYProviders -> IO ()
signAndSubmitTx txBody providers = signTransaction txBody >>= gySubmitTx providers >>= \ txId -> do
    putStrLn $ toIGreen "Signed Transaction submitted successfully"
    putStrLn "Waiting for transaction confirmation ..."
    gyAwaitTxConfirmed providers awaitParams txId >> printTxID txId
    where
        awaitParams = GYAwaitTxParameters 20 10_000_000 1

welcomeScreen :: IO ()
welcomeScreen = printMsg 100    [           "                                                                                        "
                                , toIYellow "                                Welcome To Hexxagon Game                                "
                                ,           "                                                                                        "
                                ]