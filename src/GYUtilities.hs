module  GYUtilities ( valueHasAssetClass
                    , utxoHasAssetClass
                    , getUTxOByNFT
                    , getAssets
                    , utxoHasAnyAsset
                    , gameSettingsFromUTxO
                    , playerToGYAssetClass
                    ) where

import GeniusYield.Types (GYValue, GYAssetClass, valueAssets, GYAddress, GYUTxOs, GYUTxO (utxoValue), utxosToList, GYOutDatum (..), utxoOutDatum, datumToPlutus, assetClassFromPlutus)
import Data.Set qualified as Set
import GeniusYield.TxBuilder (GYTxQueryMonadNode, GYTxQueryMonad (utxosAtAddress))
import DataTypes (Player(..), GameSettings (..))
import PlutusLedgerApi.V2 (Datum(..), fromBuiltinData)
import PlutusLedgerApi.V1.Value (AssetClass(..))
import Instances ()

valueHasAssetClass :: GYAssetClass -> GYValue -> Bool
valueHasAssetClass nft = Set.member nft . valueAssets

utxoHasAssetClass :: GYAssetClass -> GYUTxO -> Bool
utxoHasAssetClass nft = Set.member nft . valueAssets . utxoValue

getUTxOByNFT :: GYAddress -> GYAssetClass -> GYTxQueryMonadNode GYUTxOs
getUTxOByNFT addr nft = utxosAtAddress addr (Just nft)

getAssets :: GYUTxOs -> Set.Set GYAssetClass
getAssets = mconcat . ((valueAssets . utxoValue) <$>) . utxosToList

utxoHasAnyAsset :: Set.Set GYAssetClass -> GYUTxO -> Bool
utxoHasAnyAsset assets (gameSettingsFromUTxO -> Just (getPlayer1 -> BluePlayer cs tn)) =
    case assetClassFromPlutus (AssetClass (cs,tn)) of
        Right gyAssetClass -> Set.member gyAssetClass assets
        Left e -> error (show e)
utxoHasAnyAsset _ _ = False

gameSettingsFromUTxO :: GYUTxO -> Maybe GameSettings
gameSettingsFromUTxO (utxoOutDatum -> GYOutDatumInline (datumToPlutus -> Datum d)) = fromBuiltinData d
gameSettingsFromUTxO (utxoOutDatum -> _) = Nothing

playerToGYAssetClass :: Player -> GYAssetClass
playerToGYAssetClass player = case assetClassFromPlutus nft of
    Right gyNFT -> gyNFT
    Left e -> error (show e)
    where
        nft = case player of
            BluePlayer cs tn -> AssetClass (cs,tn)
            RedPlayer  cs tn -> AssetClass (cs,tn)