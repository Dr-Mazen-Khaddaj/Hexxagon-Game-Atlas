{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module  Arbitrary.Generators    ( genLovelace
                                , genNFT
                                , genSessionNumber
                                , genPaymentAddress
                                , genPaymentAddress'
                                , genScriptAddress
                                , genPaymentTxOut
                                , genScriptTxOut
                                , genPaymentTxInInfo
                                , genScriptTxInInfo
                                , genNScriptTxInInfo
                                , genTxOutFromAddrValue
                                , genTxOutExAddr
                                , genSizedValue
                                , genSizedBuiltinByteString
                                ) where

import  PlutusTx                    ( Data (..))
import  PlutusTx.Prelude            ( BuiltinByteString, consByteString )
import  PlutusTx.Builtins           ( toBuiltin, blake2b_256, fromBuiltin)
import  PlutusLedgerApi.V1.Value    ( AssetClass (AssetClass))
import  PlutusLedgerApi.V2          ( Address (Address), PubKeyHash (..), ScriptHash (..)
                                    , Credential (..), StakingCredential (..)
                                    , CurrencySymbol (..), TokenName (..), Value (..), singleton, adaSymbol, adaToken
                                    , BuiltinData (BuiltinData), DatumHash (DatumHash), Datum (Datum), OutputDatum (..)
                                    , TxOut (..), TxOutRef (..), TxId (..)
                                    , TxInInfo (..), POSIXTime (POSIXTime)
                                    )
import  PlutusTx.AssocMap           qualified as AssocMap
import  Data.ByteString.Char8       qualified as C8
import  Data.ByteString.Base16      qualified as B16
import  Data.Map                    qualified as Map
import  Data.Function               ( on )
import  Test.QuickCheck             ( Gen, Arbitrary (arbitrary), oneof, chooseInt, sample, suchThat, vectorOf, choose, resize, chooseInteger )
import  Constants                   ( thousand )
import  Arbitrary.PlutusTx          ( genListOf, assortValue )

------------------------------------------------------ | Generators | ------------------------------------------------------

genPaymentAddress' :: PubKeyHash -> Gen Address
genPaymentAddress' pkh = Address (PubKeyCredential pkh) <$> arbitrary

genPaymentAddress, genScriptAddress :: Gen Address
genPaymentAddress = Address . PubKeyCredential <$> arbitrary <*> arbitrary
genScriptAddress  = Address . ScriptCredential <$> arbitrary <*> arbitrary

genPaymentTxOut, genScriptTxOut :: Gen TxOut
genPaymentTxOut = TxOut <$> genPaymentAddress <*> arbitrary <*> arbitrary <*> arbitrary
genScriptTxOut  = TxOut <$> genScriptAddress  <*> arbitrary <*> arbitrary <*> arbitrary

genPaymentTxInInfo, genScriptTxInInfo :: Gen TxInInfo
genPaymentTxInInfo = TxInInfo <$> arbitrary <*> genPaymentTxOut
genScriptTxInInfo  = TxInInfo <$> arbitrary <*> genScriptTxOut

genNScriptTxInInfo :: Int -> Gen [TxInInfo]
genNScriptTxInInfo n = (<>) <$> genListOf 5 genPaymentTxInInfo <*> vectorOf n genScriptTxInInfo

genTxOutFromAddrValue :: Address -> Value -> Gen TxOut
genTxOutFromAddrValue addr value = TxOut addr value <$> arbitrary <*> arbitrary

genTxOutExAddr :: Address -> Gen TxOut
genTxOutExAddr addr = TxOut <$> arbitrary `suchThat` (/= addr) <*> arbitrary <*> arbitrary <*> arbitrary

genLovelace :: Gen Value
genLovelace = singleton adaSymbol adaToken <$> arbitrary `suchThat` (> 0)

genNFT :: Gen Value
genNFT = singleton <$> arbitrary <*> arbitrary <*> pure 1

genSizedValue :: Int -> Gen Value
genSizedValue 0 = pure mempty
genSizedValue (subtract 1 -> n) = assortValue <$> totalValue
    where
        totalValue = (<>) <$> ada <*> tokens
        ada = genLovelace
        tokens = Value . AssocMap.fromList <$> vectorOf n arbitraryCurrencyMapPair
        arbitraryCurrencyMapPair    = (,) <$> arbitrary <*> arbitraryTokenMap
        arbitraryTokenMap           = AssocMap.fromList <$> vectorOf n nonZeroTokenPair
        nonZeroTokenPair            = (,) <$> genTokenName <*> genPositiveNonZeroInteger
        genTokenName                = TokenName <$> genSizedBuiltinByteString 10
        genPositiveNonZeroInteger   = arbitrary `suchThat` (> 0)

genSizedBuiltinByteString :: Int -> Gen BuiltinByteString
genSizedBuiltinByteString n = foldr consByteString "" <$> vectorOf n (chooseInteger (1,256))

genSessionNumber:: Gen Integer
genSessionNumber = arbitrary `suchThat` (>= 0)

------------------------------------------------------  End of Code  -------------------------------------------------------