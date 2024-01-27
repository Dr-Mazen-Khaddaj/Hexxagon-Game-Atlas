{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary (arbitrary) where

import DataTypes (Hexagon (..), Position (Position), Board (Board))
import Test.QuickCheck (Arbitrary (arbitrary), oneof, suchThat, vectorOf, Gen, chooseInt)
import qualified Data.Map as Map

instance Arbitrary Hexagon where arbitrary = oneof $ pure <$> [Empty,Red,Blue]

instance Arbitrary Position where arbitrary = Position <$> genInt <*> genInt
                                    where genInt = arbitrary `suchThat` (\i -> i > 0 && i < 15)

instance Arbitrary Board where arbitrary = Board . Map.fromList <$> genListOf100 500 arbitrary
-- genPositiveNonZeroInteger = arbitrary `suchThat` (> 0)

_genListOf1 :: Int -> Gen a -> Gen [a]
_genListOf1 n g = chooseInt (1, n) >>= flip vectorOf g

genListOf100 :: Int -> Gen a -> Gen [a]
genListOf100 n g = chooseInt (10, n) >>= flip vectorOf g