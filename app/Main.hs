module Main (main) where

import Control.Monad.State      ( StateT (runStateT), get, liftIO )
import GeniusYield.GYConfig     ( withCfgProviders )
import GeniusYield.Types        ( GYTxId )
import Scripts                  ( gyScriptToAddress, initialiseGameSC, runGameSC, refNFTManagerSC )
import GameConfig               ( Config(Config), getGameConfig )
import IOUtilities              ( printTxID, ToColor (..), chooseIndex )
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

    config <- getGameConfig
    print config
    i <- chooseIndex @String "Action" ["Create Game", "Cancel Game" , "Mint Player NFT"]
    case i of
        0 -> runStateT createGame       config >>= printTxID . fst
        1 -> runStateT cancelGame       config >>= printTxID . fst
        2 -> runStateT mintPlayerNFT    config >>= printTxID . fst
        _ -> error "Action Not available!"

createGame :: StateT Config IO GYTxId
createGame = do
    Config coreCfg signingKey walletAddr identifierNFT Nothing <- get
    liftIO $ withCfgProviders coreCfg (toIGreen "CreateGame") $ Actions.CreateGame.action coreCfg signingKey walletAddr identifierNFT

cancelGame :: StateT Config IO GYTxId
cancelGame = do
    Config coreCfg signingKey walletAddr _ Nothing <- get
    liftIO $ withCfgProviders coreCfg (toIGreen "CancelGame") $ Actions.CancelGame.action coreCfg signingKey walletAddr

mintPlayerNFT :: StateT Config IO GYTxId
mintPlayerNFT = do
    Config coreCfg signingKey walletAddr _ Nothing <- get
    liftIO $ withCfgProviders coreCfg (toIGreen "MintPlayerNFT") $ Actions.MintPlayerNFT.action coreCfg signingKey walletAddr

welcomeScreen :: IO ()
welcomeScreen = do
    putStrLn $ toICyan "\n\t\t--------------------------------------------------------"
    putStrLn $ toICyan   "\t\t--------------- Welcome To Hexxagon Game ---------------"
    putStrLn $ toICyan   "\t\t--------------------------------------------------------\n"
