module  GYUtilities ( valueHasAssetClass
                    , utxoHasAssetClass
                    , getUTxOsByNFT
                    , getAssets
                    , availableToPlayer
                    , playerToGYAssetClass
                    , fromUTxO
                    ) where

import GeniusYield.Types (GYValue, GYAssetClass, valueAssets, GYAddress, GYUTxOs, GYUTxO (utxoValue), utxosToList, GYOutDatum (..), utxoOutDatum, datumToPlutus, assetClassFromPlutus)
import Data.Set qualified as Set
import GeniusYield.TxBuilder (GYTxQueryMonadNode, GYTxQueryMonad (utxosAtAddress))
import DataTypes (Player(..), GameSettings (..), GameInfo (getGameState), GameState (getPlayer'sTurn))
import PlutusLedgerApi.V2 (Datum(..), fromBuiltinData, FromData)
import PlutusLedgerApi.V1.Value (AssetClass(..))
import Instances ()

valueHasAssetClass :: GYAssetClass -> GYValue -> Bool
valueHasAssetClass nft = Set.member nft . valueAssets

utxoHasAssetClass :: GYAssetClass -> GYUTxO -> Bool
utxoHasAssetClass nft = Set.member nft . valueAssets . utxoValue

getUTxOsByNFT :: GYAddress -> GYAssetClass -> GYTxQueryMonadNode GYUTxOs
getUTxOsByNFT addr nft = utxosAtAddress addr (Just nft)

getAssets :: GYUTxOs -> Set.Set GYAssetClass
getAssets = mconcat . ((valueAssets . utxoValue) <$>) . utxosToList

availableToPlayer :: Set.Set GYAssetClass -> GYUTxO -> Bool
availableToPlayer assets (authNFTFromUTxO -> Just gyAssetClass) = Set.member gyAssetClass assets
availableToPlayer _ _ = False

authNFTFromUTxO :: GYUTxO -> Maybe GYAssetClass
authNFTFromUTxO (utxoOutDatum -> GYOutDatumInline (datumToPlutus -> Datum d)) = case fromBuiltinData @GameSettings d of
    Just settings -> Just $ playerToGYAssetClass settings.getPlayer1
    Nothing -> playerToGYAssetClass . getPlayer'sTurn . getGameState <$> fromBuiltinData d
authNFTFromUTxO _ = Nothing

fromUTxO :: FromData a => GYUTxO -> Maybe a
fromUTxO (utxoOutDatum -> GYOutDatumInline (datumToPlutus -> Datum d)) = fromBuiltinData d
fromUTxO (utxoOutDatum -> _) = Nothing

playerToGYAssetClass :: Player -> GYAssetClass
playerToGYAssetClass player = case assetClassFromPlutus nft of
    Right gyNFT -> gyNFT
    Left e -> error (show e)
    where
        nft = case player of
            BluePlayer cs tn -> AssetClass (cs,tn)
            RedPlayer  cs tn -> AssetClass (cs,tn)