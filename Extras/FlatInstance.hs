{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
-- {-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module FlatInstance where
-- module UntypedPlutusCore.Core.Instance.Flat where
-- https://hoogle.nix.dance/file//nix/store/wdhgglgmqi4b4qyh0jkxzix4cngwf2l8-plutus-core-lib-plutus-core-1.0.0.0.0.0.0.0.1-haddock-doc/share/doc/plutus-core/html/src/UntypedPlutusCore.Core.Instance.Flat.html#line-204

import UntypedPlutusCore.Core.Type

import PlutusCore.Flat
import PlutusCore.Pretty

import Data.Word (Word8)
import Flat
import Flat.Decoder
import Flat.Encoder
import Universe

-- | Using 4 bits to encode term tags.
termTagWidth :: NumBits
termTagWidth = 4

encodeTermTag :: Word8 -> Encoding
encodeTermTag = safeEncodeBits termTagWidth

decodeTermTag :: Get Word8
decodeTermTag = dBEBits8 termTagWidth

encodeTerm
    :: forall name uni fun ann
    . ( Closed uni
    , uni `Everywhere` Flat
    , PrettyPlc (Term name uni fun ann)
    , Flat fun
    , Flat ann
    , Flat name
    , Flat (Binder name)
    )
    => Term name uni fun ann
    -> Encoding
encodeTerm = \case
    Var      ann n    -> encodeTermTag 0 <> encode ann <> encode n
    Delay    ann t    -> encodeTermTag 1 <> encode ann <> encodeTerm t
    LamAbs   ann n t  -> encodeTermTag 2 <> encode ann <> encode (Binder n) <> encodeTerm t
    Apply    ann t t' -> encodeTermTag 3 <> encode ann <> encodeTerm t <> encodeTerm t'
    Constant ann c    -> encodeTermTag 4 <> encode ann <> encode c
    Force    ann t    -> encodeTermTag 5 <> encode ann <> encodeTerm t
    Error    ann      -> encodeTermTag 6 <> encode ann
    Builtin  ann bn   -> encodeTermTag 7 <> encode ann <> encode bn

decodeTerm
    :: forall name uni fun ann
    . ( Closed uni
    , uni `Everywhere` Flat
    , PrettyPlc (Term name uni fun ann)
    , Flat fun
    , Flat ann
    , Flat name
    , Flat (Binder name)
    )
    => (fun -> Bool)
    -> Get (Term name uni fun ann)
decodeTerm builtinPred = go
    where
        go = handleTerm =<< decodeTermTag
        handleTerm 0 = Var      <$> decode <*> decode
        handleTerm 1 = Delay    <$> decode <*> go
        handleTerm 2 = LamAbs   <$> decode <*> (unBinder <$> decode) <*> go
        handleTerm 3 = Apply    <$> decode <*> go <*> go
        handleTerm 4 = Constant <$> decode <*> decode
        handleTerm 5 = Force    <$> decode <*> go
        handleTerm 6 = Error    <$> decode
        handleTerm 7 = do
            ann <- decode
            fun <- decode
            let t :: Term name uni fun ann
                t = Builtin ann fun
            if builtinPred fun
            then pure t
            else fail $ "Forbidden builtin function: " ++ show (prettyPlcDef t)
        handleTerm t = fail $ "Unknown term constructor tag: " ++ show t

sizeTerm
    :: forall name uni fun ann
    . ( Closed uni
    , uni `Everywhere` Flat
    , PrettyPlc (Term name uni fun ann)
    , Flat fun
    , Flat ann
    , Flat name
    , Flat (Binder name)
    )
    => Term name uni fun ann
    -> NumBits
    -> NumBits
sizeTerm tm sz = termTagWidth + sz + case tm of
    Var      ann n    -> getSize ann + getSize n
    Delay    ann t    -> getSize ann + getSize t
    LamAbs   ann n t  -> getSize ann + getSize n + getSize t
    Apply    ann t t' -> getSize ann + getSize t + getSize t'
    Constant ann c    -> getSize ann + getSize c
    Force    ann t    -> getSize ann + getSize t
    Error    ann      -> getSize ann
    Builtin  ann bn   -> getSize ann + getSize bn

decodeProgram
    :: forall name uni fun ann
    . ( Closed uni
    , uni `Everywhere` Flat
    , PrettyPlc (Term name uni fun ann)
    , Flat fun
    , Flat ann
    , Flat name
    , Flat (Binder name)
    )
    => (fun -> Bool)
    -> Get (Program name uni fun ann)
decodeProgram builtinPred = Program <$> decode <*> decode <*> decodeTerm builtinPred

{- Note [Deserialization on the chain]
As discussed in Note [Deserialization size limits], we want to limit how big constants are when deserializing.
But the 'Flat' instances for plain terms and programs provided here don't do that: they implement unrestricted deserialization.

In practice we use a specialized decoder for the on-chain decoding which calls 'decodeProgram' directly.
Possibly we should remove these instances in future and only have instances for newtypes that clearly communicate
the expected behaviour.
-}

instance ( Closed uni
         , uni `Everywhere` Flat
         , PrettyPlc (Term name uni fun ann)
         , Flat fun
         , Flat ann
         , Flat name
         , Flat (Binder name)
         ) => Flat (Term name uni fun ann) where
    encode = encodeTerm
    decode = decodeTerm (const True)
    size = sizeTerm

-- This instance could probably be derived, but better to write it explicitly ourselves so we have control!
instance ( Closed uni
         , uni `Everywhere` Flat
         , PrettyPlc (Term name uni fun ann)
         , Flat fun
         , Flat ann
         , Flat name
         , Flat (Binder name)
         ) => Flat (Program name uni fun ann) where
    encode (Program ann v t) = encode ann <> encode v <> encode t

    size (Program a v t) n = n + getSize a + getSize v + getSize t