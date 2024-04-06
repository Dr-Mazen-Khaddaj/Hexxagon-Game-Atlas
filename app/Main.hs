module Main where

import Control.Concurrent
import Control.Concurrent.Async (async)
import RunServer (runServer)
import DAppConfig (Config(..))
import GetConfig (getConfig, updateConfig)
import Control.Monad.Trans.State (StateT, runStateT, gets, get, put)
import qualified Actions.CreateGame
import qualified Actions.CancelGame
import qualified Actions.JoinGame
import qualified Actions.RunGame
import qualified Actions.MintPlayerNFT
import GeniusYield.Types (GYProviders (..), GYTxBody, GYAwaitTxParameters (GYAwaitTxParameters))
import Control.Monad.IO.Class (liftIO)
import IOUtilities (chooseIndex, printTxID, ToColor (..), printMsg, UnquotedString (Unquoted))
import GeniusYield.GYConfig (withCfgProviders)
import SignTransaction (signTransaction)
import Control.Monad (unless)

main :: IO ()
main = do
    welcomeScreen
    usedAddrsVar        <- newEmptyMVar
    changeAddrVar       <- newEmptyMVar
    unsignedTxVar       <- newEmptyMVar
    signedWitnessVar    <- newEmptyMVar
    _serverThread       <- async $ runServer usedAddrsVar changeAddrVar unsignedTxVar signedWitnessVar
    config              <- getConfig
    _ <- runStateT runDApp config
    pure ()

runDApp :: StateT Config IO ()
runDApp = do
    coreCfg <- gets getCoreConfig
    i <- liftIO $ chooseIndex "Action" $ Unquoted <$> ["Create Game", "Cancel Game" , "Join Game" , "Run Game" , "Mint Player NFT", "Exit"]
    unless (i == 5) $ do
        action <- case i of
            0 -> buildTxBody "CreateGame"       Actions.CreateGame.action
            1 -> buildTxBody "CancelGame"       Actions.CancelGame.action
            2 -> buildTxBody "JoinGame"         Actions.JoinGame.action
            3 -> buildTxBody "RunGame"          Actions.RunGame.action
            4 -> buildTxBody "MintPlayerNFT"    Actions.MintPlayerNFT.action
            _ -> error "Action Not available!"
        case action of
            Right txBody -> do
                liftIO $ withCfgProviders coreCfg (toIGreen "SubmitTx") $ signAndSubmitTx txBody
                newConfig <- get >>= liftIO . updateConfig
                put newConfig >> runDApp
            Left e -> liftIO (printErrorMsgs e) >> runDApp

buildTxBody :: String -> (Config -> GYProviders -> IO (Either [String] GYTxBody)) -> StateT Config IO (Either [String] GYTxBody)
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

printRetrievedAddresses :: Config -> IO ()
printRetrievedAddresses (Config _ walletAddresses changeAddress _ _) = do
    putStrLn "Retrieved wallet addresses: " >> mapM_ print walletAddresses
    putStrLn "Change address: " >> print changeAddress

printErrorMsgs :: [String] -> IO ()
printErrorMsgs = mapM_ (putStrLn . toRed)

welcomeScreen :: IO ()
welcomeScreen = printMsg 100    [           "                                                                                        "
                                , toIYellow "                                Welcome To Hexxagon Game                                "
                                ,           "                                                                                        "
                                ]