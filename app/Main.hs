module Main (main) where

import GeniusYield.GYConfig     (withCfgProviders)
import Scripts                  ( gyScriptToAddress, initialiseGameSC, runGameSC )
import GameConfig               ( Config(Config), getGameConfig )
import Actions.CreateGame       qualified
-- import Actions.CancelGame       qualified

main :: IO ()
main = do
    initialiseGameSC        >>= print
    runGameSC               >>= print
    initialiseGameSC        >>= print . gyScriptToAddress
    runGameSC               >>= print . gyScriptToAddress

    config@(Config coreCfg signingKey walletAddr identifierNFT Nothing) <- getGameConfig
    print config
    txId <- withCfgProviders coreCfg "[Create Game]" $ Actions.CreateGame.action coreCfg signingKey walletAddr identifierNFT
    -- txId <- withCfgProviders coreCfg "[Cancel Game]" $ Actions.CancelGame.action coreCfg signingKey walletAddr
    print txId