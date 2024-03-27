{-# LANGUAGE OverloadedStrings #-}

module SignTransaction (signTransaction) where

import Network.HTTP.Simple
import Servant                  (parseHeader)
import Control.Concurrent       (threadDelay)
import GeniusYield.Types        (GYTx, GYTxBody, unsignedTx, txToHexBS, GYTxWitness, appendWitnessGYTx)
import Data.Text                qualified as T
import Data.ByteString.Lazy     qualified as BL
import Data.ByteString          qualified as B
import IOUtilities              (printMsg, ToColor (..))

signTransaction :: GYTxBody -> IO GYTx
signTransaction txBody = do
    let unsignedTransaction = unsignedTx txBody
    postUnsignedTransaction unsignedTransaction
    printMsg 74 . (:[]) $ "Please Sign the Transaction to proceed : " <> toICyan "http://localhost:3000"
    putStrLn "Waiting for witness ..."
    witness <- pollForWitness
    putStrLn $ toIGreen "Witness received"
    pure $ appendWitnessGYTx witness unsignedTransaction

postUnsignedTransaction :: GYTx -> IO ()
postUnsignedTransaction tx = do
    let txHex = txToHexBS tx
    let requestBody = "unsignedTx=" <> B.fromStrict txHex
    request <-  setRequestMethod "POST"
            .   setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
            .   setRequestBodyLBS requestBody
            <$> parseRequest "http://localhost:3000/submit-unsigned-transaction"
    response <- httpNoBody request
    if getResponseStatusCode response == 200
        then putStrLn $ toIGreen "Unsigned Transaction posted successfully"
        else do
            putStrLn $ "Response status: " ++ show (getResponseStatusCode response)
            putStrLn $ toRed "Failed to post Unsigned Transaction!"
            putStrLn "Retrying after 5 seconds ..."
            threadDelay 5_000_000 >> postUnsignedTransaction tx

pollForWitness :: IO GYTxWitness
pollForWitness = do
    let request = setRequestMethod "GET"
                $ setRequestPath "/get-witness"
                $ setRequestHost "localhost"
                $ setRequestPort 3000 defaultRequest
    response <- httpLBS request
    let witness = getResponseBody response
    if witness == "No witness received yet"
        then threadDelay 1_000_000 >> pollForWitness
        else return $ parseWitness witness

parseWitness :: BL.ByteString -> GYTxWitness
parseWitness witnessHex = case parseHeader $ BL.toStrict witnessHex of
    Right witness -> witness
    Left str -> error $ T.unpack str
