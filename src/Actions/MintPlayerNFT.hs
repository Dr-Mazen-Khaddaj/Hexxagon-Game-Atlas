module Actions.MintPlayerNFT (action) where

import GeniusYield.Types
import GeniusYield.TxBuilder
import GeniusYield.GYConfig
import PlutusLedgerApi.V2
import UtilityFxs           ( bytesFromHex )
import DataTypes            ( Metadata (..) )
import Instances            ()
import Scripts              qualified
import PlutusTx.AssocMap    qualified as AssocMap
import DAppConfig           (Config (..))

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYMintScript 'PlutusV2 -> GYTxOutRef -> GYAddress -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton playerIdentifierMP theUniqueTxOutRef refNFTManagerSCAddress = pure
    $  mustHaveInput  (GYTxIn theUniqueTxOutRef GYTxInWitnessKey)
    <> mustHaveOutput (GYTxOut refNFTManagerSCAddress (refNFT <> q2ADA) (Just (metadataData , GYTxOutUseInlineDatum)) Nothing)
    <> mustMint playerIdentifierMP unitRedeemer userNFT_Name 1
    <> mustMint playerIdentifierMP unitRedeemer refNFT_Name  1

    where
        playerIdentifierMPID = mintingPolicyIdFromWitness playerIdentifierMP

        userNFTLabel    = bytesFromHex "000de140"
        refNFTLabel     = bytesFromHex "000643b0"

        userNFT_Name    = GYTokenName $ userNFTLabel <> "HEXXAGON"
        refNFT_Name     = GYTokenName $ refNFTLabel  <> "HEXXAGON"

        q2ADA           = valueFromLovelace 2_000_000
        refNFT          = valueSingleton (GYToken playerIdentifierMPID refNFT_Name)  1

        metadataData = datumFromPlutusData metadata
        metadata = Metadata hexxagonMetadata 2 (List [])
        hexxagonMetadata = AssocMap.fromList    [ ("name"           , B "Hexxagon Game"                                          )
                                                , ("image"          , B "ipfs://QmcNdgGVQ5Yw9ckWH2PohYKyvmud2MaksyzHz1SBAoM89h"  )
                                                , ("description"    , B "Hexxagon game on the Cardano Blockchain"                )
                                                , ("authors"        , B "Dr. Mazen Khaddaj & Andrew Garrett Wright"              )
                                                , ("score"          , I 0                                                        )
                                                ]

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: Config -> GYProviders -> IO GYTxBody
action (Config coreCfg walletAddrs changeAddr _ _) providers = do
    Just (theUniqueTxOutRef ,_) <- query (utxosAtAddresses walletAddrs) >>= randomTxOutRef
    playerIdentifierMP          <- Scripts.playerIdentifierMP theUniqueTxOutRef
    refNFTManagerSCAddress      <- Scripts.gyScriptToAddress <$> Scripts.refNFTManagerSC
    runTx $ skeleton playerIdentifierMP theUniqueTxOutRef refNFTManagerSCAddress
    where
        networkID = cfgNetworkId coreCfg
        query = runGYTxQueryMonadNode networkID providers
        runTx = runGYTxMonadNode networkID providers walletAddrs changeAddr Nothing

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |