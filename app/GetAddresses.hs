{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GetAddresses (fetchWalletAddresses) where

import Network.HTTP.Simple
import Network.HTTP.Types           ( urlDecode   )
import Servant                      ( parseHeader )
import Data.Either                  ( fromRight   )
import Data.List.Split              ( splitOn     )
import Control.Concurrent           ( threadDelay )
import GeniusYield.Types.Address    ( GYAddress   )
import IOUtilities                  ( ToColor(..), printMsg )
import Data.ByteString              qualified as B
import Data.ByteString.Lazy         qualified as BL
import Data.ByteString.Lazy.Char8   qualified as BC8

fetchWalletAddresses :: IO ([GYAddress],GYAddress)
fetchWalletAddresses = do
    printMsg 76 . (:[]) $ "Please Fetch Wallet Addresses to proceed : " <> toICyan "http://localhost:3000"
    putStrLn "Waiting for addresses ..."
    addresses <- pollForAddresses
    putStrLn $ toIGreen "Addresses retrieved successfully"
    pure addresses

pollForAddresses :: IO ([GYAddress],GYAddress)
pollForAddresses = do
    let request = setRequestMethod "GET"
                $ setRequestPath "/get-addresses"
                $ setRequestHost "localhost"
                $ setRequestPort 3000 defaultRequest
    response <- httpLBS request
    let addressesStr = getResponseBody response
    if addressesStr == "No addresses received yet"
        then threadDelay 1_000_000 >> pollForAddresses
        else return $ parseAddresses addressesStr

parseAddresses :: BL.ByteString -> ([GYAddress],GYAddress)
parseAddresses str = (,) usedGYAddrs changeGYAddr
    where
        parseAddrs addrs = [fromRight (error "Not an address! @parseAddresses") $ parseHeader addr | addr <- addrs , B.head addr == 48]
        [usedGYAddrs,[changeGYAddr]] = parseAddrs . fmap (BC8.toStrict . BC8.pack) . splitOn ","
            <$> (splitOn ";" . BC8.unpack . B.fromStrict . urlDecode True $ BL.toStrict str)
