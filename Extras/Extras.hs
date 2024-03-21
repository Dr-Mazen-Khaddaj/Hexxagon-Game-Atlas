{-# OPTIONS_GHC -Wno-unused-imports #-}

module Extras (fromCardanoPlutusScript, applyArguments, serializableToScript) where

-- import Prelude qualified as Haskell
import Codec.Serialise ( Serialise, decode, encode, deserialise, serialise )
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import UntypedPlutusCore                            qualified as UPLC
import UntypedPlutusCore.Check.Scope                qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek     qualified as UPLC
import PlutusCore                                   qualified as PLC
import PlutusCore.Data                              qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget       qualified as PLC
import PlutusCore.MkPlc                             qualified as PLC

import Codec.CBOR.Decoding qualified as CBOR
import Data.Either.Extras ( fromRightM )
import Flat qualified
import Flat.Decoder qualified as Flat

import qualified Data.ByteString.Lazy as BSL

import PlutusLedgerApi.V2 (SerialisedScript)
import Data.ByteString.Short (fromShort)

import qualified Cardano.Api as C
import qualified Codec.Serialise as Codec

import qualified Data.ByteString.Short as BSS

import FlatInstance ()
import PlutusPrelude (over)
import Cardano.Api (PlutusScript, PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript(PlutusScriptSerialised))
--------------------------------------------------------------------------------------------------------------------------- |

newtype Script = Script { unScript :: UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun () }
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving Serialise via (SerialiseViaFlat (UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()))

--------------------------------------------------------------------------------------------------------------------------- |
-- Codec.CBOR.Extras
-- https://hoogle.nix.dance/file/nix/store/0bbbwbsg0pfbgx5a6vs8bb1b2lj5m6x1-plutus-ledger-api-lib-plutus-ledger-api-1.0.0.0.0.0.0.0.1-haddock-doc/share/doc/plutus-ledger-api/html/src/Codec.CBOR.Extras.html#SerialiseViaFlat

newtype SerialiseViaFlat a = SerialiseViaFlat a
instance Flat.Flat a => Serialise (SerialiseViaFlat a) where
    encode (SerialiseViaFlat a) = encode $ Flat.flat a
    decode = SerialiseViaFlat <$> decodeViaFlat Flat.decode

decodeViaFlat :: Flat.Get a -> CBOR.Decoder s a
decodeViaFlat decoder = do
    bs <- CBOR.decodeBytes
    fromRightM (fail . show) $ Flat.unflatWith decoder bs

--------------------------------------------------------------------------------------------------------------------------- |

-- deserialiseUPLC :: SerialisedScript -> Program DeBruijn DefaultUni DefaultFun ()
-- deserialiseUPLC = unSerialiseViaFlat . deserialise . BSL.fromStrict . fromShort
--   where
--     unSerialiseViaFlat (SerialiseViaFlat a) = a

fromCardanoPlutusScript :: C.HasTypeProxy lang => C.PlutusScript lang -> Script
fromCardanoPlutusScript = Codec.deserialise . BSL.fromStrict . C.serialiseToRawBytes


applyArguments :: Script -> [PLC.Data] -> Script
applyArguments (Script p) args =
    let termArgs = fmap (PLC.mkConstant ()) args
        applied t = PLC.mkIterApp () t termArgs
    in Script $ over UPLC.progTerm applied p

serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

--------------------------------------------------------------------------------------------------------------------------- |