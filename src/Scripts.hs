module Scripts  ( initialiseGameSC
                , runGameSC
                , refNFTManagerSC
                , gyScriptToValidator
                , gyScriptToAddress
                , playerIdentifierMP
                ) where

import GeniusYield.Types
import PlutusLedgerApi.V2   ( Credential (..), Address (..), Data, toData )
import Data.Aeson           ( Value (..), decode )
import Data.Aeson.KeyMap    ( toList )
import Data.Maybe           ( fromMaybe )
import Extras               ( applyArguments, fromCardanoPlutusScript, serializableToScript )
import Data.ByteString.Lazy qualified as LBS

------------------------------------------------------- | Scripts | ------------------------------------------------------- |
initialiseGameSC, runGameSC, refNFTManagerSC :: IO (GYScript 'PlutusV2)

initialiseGameSC    = getScriptFromFile "InitialiseGameSC"
runGameSC           = getScriptFromFile "RunGameSC"
refNFTManagerSC     = getScriptFromFile "RefNFTManagerSC"

playerIdentifierMP :: GYTxOutRef -> IO (GYMintScript 'PlutusV2)
playerIdentifierMP = (<$> getScriptFromFile "PlayerIdentifierMP") . ((GYMintScript .) . gyScriptToMintingPolicy) . (:[]) . toData . txOutRefToPlutus

--------------------------------------------------- | Helper Functions | -------------------------------------------------- |

gyScriptToValidator :: SingPlutusVersionI v => GYScript v -> GYValidator v
gyScriptToValidator = validatorFromApi . scriptToApi

gyScriptToAddress :: GYScript v -> GYAddress
gyScriptToAddress script = case addressFromPlutus GYTestnetPreview . flip Address Nothing . ScriptCredential $ scriptPlutusHash script of
    Right a -> a
    Left  e -> error (show e)

getScriptFromFile :: forall (v :: PlutusVersion). SingPlutusVersionI v => String -> IO (GYScript v)
getScriptFromFile name = do
    Just (Object obj) <- decode @Value <$> LBS.readFile ("Scripts/" <> name <> ".json")
    let script = case fromMaybe (error "Invalid key \"cborHex\"!") (lookup "cborHex" $ toList obj) of
            String cbor -> fromMaybe (error "Can't get Script from CBOR!") $ scriptFromCBOR cbor
            _ -> error "Value is not 'String'!"
    return script

gyScriptToMintingPolicy :: [Data] -> GYScript 'PlutusV2 -> GYMintingPolicy 'PlutusV2
gyScriptToMintingPolicy args script = mintingPolicyFromApi $ serializableToScript $ applyArguments (fromCardanoPlutusScript (scriptToApi script)) args

--------------------------------------------------------------------------------------------------------------------------- |