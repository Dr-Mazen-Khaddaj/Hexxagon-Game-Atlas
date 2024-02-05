{-# OPTIONS_GHC -Wno-orphans #-}

module  Arbitrary.PlutusTx  ( genHash
                            , genListOf
                            , genListOf1
                            , assortValue
                            ) where

import  PlutusTx                    ( Data (..))
import  PlutusTx.Prelude            ( BuiltinByteString, consByteString )
import  PlutusTx.Builtins           ( toBuiltin, blake2b_256)
import  PlutusLedgerApi.V1.Value    ( AssetClass (AssetClass))
import  PlutusLedgerApi.V2          ( Address (Address), PubKeyHash (..), ScriptHash (..)
                                    , Credential (..), StakingCredential (..)
                                    , CurrencySymbol (..), TokenName (..), Value (..), adaSymbol, adaToken
                                    , BuiltinData (BuiltinData), DatumHash (DatumHash), Datum (Datum), OutputDatum (..)
                                    , TxOut (..), TxOutRef (..), TxId (..)
                                    , POSIXTime (POSIXTime)
                                    )
import  PlutusTx.AssocMap           qualified as AssocMap
import  Data.ByteString.Char8       qualified as C8
import  Data.Map                    qualified as Map
import  Data.Function               ( on )
import  Test.QuickCheck             ( Gen, Arbitrary (arbitrary), oneof, chooseInt, suchThat, vectorOf, chooseInteger, resize )
import  Constants                   ( thousand )

-------------------------------------------------- | Utility Functions | ---------------------------------------------------

genHash :: Gen BuiltinByteString
genHash = blake2b_256 <$> resize thousand arbitrary

assortValue :: Value -> Value
assortValue v = Value . AssocMap.fromList . Map.toAscList
              $ AssocMap.fromList . Map.toAscList . Map.fromListWith (+) . AssocMap.toList
                <$> (Map.fromListWith ((AssocMap.fromList .) . on (<>) AssocMap.toList) . AssocMap.toList $ getValue v)

genListOf, genListOf1 :: Int -> Gen a -> Gen [a]
genListOf  n g = chooseInt (0,n) >>= flip vectorOf g
genListOf1 n g = chooseInt (1,n) >>= flip vectorOf g

-------------------------------------------------- | Arbitrary Instance | --------------------------------------------------

instance Arbitrary C8.ByteString where arbitrary = C8.pack <$> arbitrary
instance Arbitrary BuiltinByteString where arbitrary = toBuiltin . C8.pack <$> arbitrary

instance Arbitrary PubKeyHash where arbitrary = PubKeyHash <$> genHash
instance Arbitrary ScriptHash where arbitrary = ScriptHash <$> genHash

-- Credentials
instance Arbitrary Credential where arbitrary = oneof   [ PubKeyCredential <$> arbitrary
                                                        , ScriptCredential <$> arbitrary
                                                        ]
instance Arbitrary StakingCredential where arbitrary = StakingHash . PubKeyCredential <$> arbitrary

-- Address
instance Arbitrary Address where arbitrary = Address <$> arbitrary <*> arbitrary

-- Tx Id
instance Arbitrary TxId where arbitrary = TxId <$> genHash
instance Arbitrary TxOutRef where arbitrary = (. abs) <$> (TxOutRef <$> arbitrary) <*> arbitrary

instance Arbitrary TxOut where arbitrary = TxOut <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- Value
instance Arbitrary CurrencySymbol where arbitrary = CurrencySymbol <$> genHash
instance Arbitrary TokenName where arbitrary = TokenName . foldr consByteString "" <$> genListOf1 32 (chooseInteger (1,256))
instance Arbitrary AssetClass where arbitrary = AssetClass <$> arbitrary

instance Arbitrary (AssocMap.Map CurrencySymbol (AssocMap.Map TokenName Integer)) where
    arbitrary = AssocMap.fromList <$> genListOf1 5 arbitraryCurrencyMapPair
        where
            arbitraryCurrencyMapPair    = oneof [ (,) <$> arbitrary <*> arbitraryTokenMap
                                                , (adaSymbol,) . AssocMap.singleton adaToken <$> genPositiveNonZeroInteger
                                                ]
            arbitraryTokenMap           = AssocMap.fromList <$> genListOf1 5 nonZeroTokenPair
            nonZeroTokenPair            = (,) <$> arbitrary <*> genPositiveNonZeroInteger
            genPositiveNonZeroInteger   = arbitrary `suchThat` (> 0)

instance Arbitrary Value where arbitrary = assortValue . Value <$> arbitrary

-- OutputDatum
instance Arbitrary DatumHash where arbitrary = DatumHash <$> genHash
instance Arbitrary BuiltinData where arbitrary = BuiltinData <$> arbitrary
instance Arbitrary Datum where arbitrary = Datum <$> arbitrary
instance Arbitrary OutputDatum where arbitrary = oneof  [ pure NoOutputDatum
                                                        , OutputDatumHash <$> arbitrary
                                                        , OutputDatum <$> arbitrary
                                                        ]

-- Data
instance Arbitrary Data where arbitrary = chooseInt (0,3) >>= arbitraryData

arbitraryData :: Int -> Gen Data
arbitraryData 0 = oneof [genI, genB]
    where
        genI = I <$> arbitrary
        genB = B <$> arbitrary

arbitraryData n = oneof [genConstr n, genMap n, genList n, genI, genB]
    where
        genI = I <$> arbitrary
        genB = B <$> arbitrary
        genConstr size = Constr <$> arbitrary <*> genListOf1 3 (arbitraryData (size - 1))
        genMap    size = Map    <$> genListOf1 3 ((,) <$> arbitraryData (size - 1) <*> arbitraryData (size - 1))
        genList   size = List   <$> genListOf1 3 (arbitraryData (size - 1))

-- POSIXTime
instance Arbitrary POSIXTime where arbitrary = POSIXTime <$> arbitrary `suchThat` (> 0)

------------------------------------------------------  End of Code  -------------------------------------------------------