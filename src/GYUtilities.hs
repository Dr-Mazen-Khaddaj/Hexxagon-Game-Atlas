module  GYUtilities ( valueHasAssetClass
                    , utxoHasAssetClass
                    , getUTxOByNFT
                    , getAssets
                    , utxoHasAnyAsset
                    , utxoGameHasAnyAsset
                    , gameSettingsFromUTxO
                    , playerToGYAssetClass
                    ) where

import GeniusYield.Types (GYValue, GYAssetClass, valueAssets, GYAddress, GYUTxOs, GYUTxO (utxoValue), utxosToList, GYOutDatum (..), utxoOutDatum, datumToPlutus, assetClassFromPlutus)
import Data.Set qualified as Set
import GeniusYield.TxBuilder (GYTxQueryMonadNode, GYTxQueryMonad (utxosAtAddress))
import DataTypes (Player(..), GameSettings (..), GameInfo (getGameState), GameState (getPlayer'sTurn))
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

-- Need to be renamed!
utxoHasAnyAsset :: Set.Set GYAssetClass -> GYUTxO -> Bool
utxoHasAnyAsset assets (gameSettingsFromUTxO -> Just (getPlayer1 -> BluePlayer cs tn)) =
    case assetClassFromPlutus (AssetClass (cs,tn)) of
        Right gyAssetClass -> Set.member gyAssetClass assets
        Left e -> error (show e)
utxoHasAnyAsset _ _ = False

gameSettingsFromUTxO :: GYUTxO -> Maybe GameSettings
gameSettingsFromUTxO (utxoOutDatum -> GYOutDatumInline (datumToPlutus -> Datum d)) = fromBuiltinData d
gameSettingsFromUTxO (utxoOutDatum -> _) = Nothing

utxoGameHasAnyAsset :: Set.Set GYAssetClass -> GYUTxO -> Bool
utxoGameHasAnyAsset assets (player'sTurnFromUTxO -> player) =
    case assetClassFromPlutus (AssetClass (cs,tn)) of
        Right gyAssetClass -> Set.member gyAssetClass assets
        Left e -> error (show e)
        where
            (cs,tn) = case player of Just (BluePlayer a b) -> (a,b)
                                     Just (RedPlayer  a b) -> (a,b)
                                     Nothing -> error "Can't get player's Turn From UTxO!"

player'sTurnFromUTxO :: GYUTxO -> Maybe Player
player'sTurnFromUTxO (utxoOutDatum -> GYOutDatumInline (datumToPlutus -> Datum d)) = getPlayer'sTurn . getGameState <$> fromBuiltinData d
player'sTurnFromUTxO (utxoOutDatum -> _) = Nothing

playerToGYAssetClass :: Player -> GYAssetClass
playerToGYAssetClass player = case assetClassFromPlutus nft of
    Right gyNFT -> gyNFT
    Left e -> error (show e)
    where
        nft = case player of
            BluePlayer cs tn -> AssetClass (cs,tn)
            RedPlayer  cs tn -> AssetClass (cs,tn)