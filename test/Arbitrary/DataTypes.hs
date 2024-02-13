{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary.DataTypes where

import  DataTypes
import  Arbitrary.PlutusDT  ()
import  PlutusTx.AssocMap   qualified as AssocMap
import  Data.Map            qualified as Map
import  Test.QuickCheck     (Arbitrary (arbitrary), oneof, suchThat, vectorOf, Gen, chooseInt)

instance Arbitrary Player           where arbitrary = oneof $ (<*> arbitrary) . (<$> arbitrary) <$> [RedPlayer,BluePlayer]
instance Arbitrary Hexagon          where arbitrary = oneof $ pure <$> [Empty,Red,Blue]
instance Arbitrary Position         where arbitrary = Position <$> genInt <*> genInt where genInt = arbitrary `suchThat` (\i -> i > 0 && i < 15)
instance Arbitrary Move             where arbitrary = Move <$> arbitrary <*> arbitrary
instance Arbitrary Board            where arbitrary = Board . AssocMap.fromList . Map.toList . Map.fromList <$> genListOf100 500 arbitrary
instance Arbitrary GameSettings     where arbitrary = Settings <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary GameState        where arbitrary = Game <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary GameInfo         where arbitrary = GameInfo <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary Initialization   where arbitrary = oneof [Add <$> arbitrary , pure Withdraw]
instance Arbitrary RunGame          where arbitrary = oneof [PlayTurn <$> arbitrary , GameOver <$> arbitrary , pure TimeOut]

genListOf100 :: Int -> Gen a -> Gen [a]
genListOf100 n g = chooseInt (10, n) >>= flip vectorOf g