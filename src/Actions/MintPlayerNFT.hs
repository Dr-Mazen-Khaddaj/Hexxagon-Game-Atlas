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

--------------------------------------------------------------------------------------------------------------------------- |
------------------------------------------------- | Transaction Skeleton | ------------------------------------------------ |

skeleton :: GYMintScript 'PlutusV2 -> GYTxOutRef -> GYAddress -> GYTxMonadNode (GYTxSkeleton 'PlutusV2)
skeleton playerIdentifierMP theUniqueTxOutRef refNFTManagerSCAddress = pure
    $  mustHaveInput  (GYTxIn theUniqueTxOutRef GYTxInWitnessKey)
    <> mustHaveOutput (GYTxOut refNFTManagerSCAddress (refNFT <> q2ADA) (Just (metadataData , GYTxOutUseInlineDatum)) Nothing)
    <> mustMint playerIdentifierMP unitRedeemer userNFT_Name 1
    <> mustMint playerIdentifierMP unitRedeemer refNFT_Name  1

    <> mustHaveOutput (GYTxOut namiWallet (userNFT <> q2ADA) Nothing Nothing)

    where
        playerIdentifierMPID = mintingPolicyIdFromWitness playerIdentifierMP

        userNFTLabel    = bytesFromHex "000de140"
        refNFTLabel     = bytesFromHex "000643b0"

        userNFT_Name    = GYTokenName $ userNFTLabel <> "HEXXAGON"
        refNFT_Name     = GYTokenName $ refNFTLabel  <> "HEXXAGON"

        q2ADA           = valueFromLovelace 2_000_000
        refNFT          = valueSingleton (GYToken playerIdentifierMPID refNFT_Name)  1
        userNFT         = valueSingleton (GYToken playerIdentifierMPID userNFT_Name) 1

        metadataData = datumFromPlutusData metadata
        metadata = Metadata hexxagonMetadata 2 (List [])
        hexxagonMetadata = AssocMap.fromList    [ ("name"           , B "Hexxagon Game"                                          )
                                                , ("image"          , B "ipfs://QmcNdgGVQ5Yw9ckWH2PohYKyvmud2MaksyzHz1SBAoM89h"  )
                                                , ("description"    , B "Hexxagon game on the Cardano Blockchain"                )
                                                , ("authors"        , B "Dr. Mazen Khaddaj & Andrew Garrett Wright"              )
                                                , ("score"          , I 0                                                        )
                                                ]

        namiWallet = unsafeAddressFromText "addr_test1qq9hmyg6p6g3s7ufrffd9c8u55e90r3h75sn9y3umjdnhlqwdlgtx98p7x7agm5fc9gf4fdx8mh3z82kza0uz4qlhywsqmuzdd"

--------------------------------------------------------------------------------------------------------------------------- |
-------------------------------------------------- | Action Definition | -------------------------------------------------- |

action :: GYCoreConfig -> GYPaymentSigningKey -> GYAddress -> GYProviders -> IO GYTxId
action    coreCfg         walletSkey             walletAddr     providers  = do
    Just (theUniqueTxOutRef ,_) <- query (utxosAtAddress walletAddr Nothing) >>= randomTxOutRef
    playerIdentifierMP          <- Scripts.playerIdentifierMP theUniqueTxOutRef
    refNFTManagerSCAddress      <- Scripts.gyScriptToAddress <$> Scripts.refNFTManagerSC
    txBody <- runGYTxMonadNode networkID providers [walletAddr] walletAddr Nothing $ skeleton playerIdentifierMP theUniqueTxOutRef refNFTManagerSCAddress
    print playerIdentifierMP
    gySubmitTx providers $ signGYTxBody txBody [walletSkey]
    where
        networkID = cfgNetworkId coreCfg
        query = runGYTxQueryMonadNode networkID providers

--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |
--------------------------------------------------------------------------------------------------------------------------- |