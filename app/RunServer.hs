module RunServer (runServer) where

import Web.Scotty               ( file, get, param, post, scotty, text )
import Control.Concurrent       ( putMVar, readMVar, tryReadMVar, MVar, tryTakeMVar )
import Control.Monad.IO.Class   ( liftIO )
import Data.Text.Lazy           qualified as T

runServer :: MVar String -> MVar String -> MVar String -> MVar String -> IO ()
runServer usedAddrsVar changeAddrVar unsignedTxVar signedWitnessVar = scotty 3000 $ do
    get "/"             $ file "app/Browser/index.html"
    get "/script.js"    $ file "app/Browser/script.js"
    get "/style.css"    $ file "app/Browser/style.css"

-- Get Wallet Addresses
    -- Endpoint to receive addresses
    post "/submit-addresses" $ do
        usedAddresses <- param "usedAddresses"
        changeAddress <- param "changeAddress"
        liftIO $ putMVar usedAddrsVar usedAddresses
        liftIO $ putMVar changeAddrVar changeAddress
        text "addresses received"

    -- Endpoint to get wallet addresses
    get "/get-addresses" $ do
        usedAddrsM <- liftIO $ tryReadMVar usedAddrsVar
        changeAddrM <- liftIO $ tryReadMVar changeAddrVar
        case (usedAddrsM,changeAddrM) of
            (Just usedAddrs, Just changeAddr) -> text $ T.pack $ usedAddrs <> ";" <> changeAddr
            _ -> text "No addresses received yet"

-- Sign the Transaction
    -- Endpoint to submit the unsigned transaction
    post "/submit-unsigned-transaction" $ do
        unsignedTx <- param "unsignedTx"
        liftIO $ putMVar unsignedTxVar unsignedTx
        text "Unsigned transaction submitted"

    -- Endpoint to serve the unsigned transaction
    get "/get-unsigned-transaction" $ do
        unsignedTx <- liftIO $ readMVar unsignedTxVar
        text $ T.pack unsignedTx

    -- Endpoint to receive the signed witness
    post "/submit-signed-witness" $ do
        signedWitness <- param "witness"
        liftIO $ putMVar signedWitnessVar signedWitness
        text "Signed witness received"

    -- Endpoint to get the witness
    get "/get-witness" $ do
        witnessTxM <- liftIO $ tryReadMVar signedWitnessVar
        case witnessTxM of
            Just witnessTx -> text $ T.pack witnessTx
            Nothing -> text "No witness received yet"

-- Reset all MVars
    post "/reset-all" $ do
        _ <- liftIO $ mapM_ tryTakeMVar [usedAddrsVar, changeAddrVar, unsignedTxVar, signedWitnessVar]
        text "All MVars reset"
