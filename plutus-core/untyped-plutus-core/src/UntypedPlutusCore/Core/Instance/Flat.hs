{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UntypedPlutusCore.Core.Instance.Flat where

import           UntypedPlutusCore.Core.Type

import           PlutusCore.Flat

import           Data.Word                   (Word8)
import           Flat
import           Flat.Decoder
import           Flat.Encoder
import           Universe

{-
The definitions in this file rely on some Flat instances defined for typed plutus core.
You can find those in PlutusCore.Flat.
-}

{- Note [Stable encoding of PLC]
READ THIS BEFORE TOUCHING ANYTHING IN THIS FILE

We need the encoding of PLC on the blockchain to be *extremely* stable. It *must not* change
arbitrarily, otherwise we'll be unable to read back old transactions and validate them.

Consequently we don't use the derivable instances of `Flat` for the PLC types that go
on the chain.

However, the instances in this file *are* constrained by instances for names, type names,
and annotations. What's to stop the instances for *those* changing, thus changing
the overall encoding on the chain?

The answer is that what goes on the chain is *always* a `Program TyName Name ()`. The instances
for `TyName` and `Name` are nailed down here, and the instance for `()` is standard.

However, having this flexibility allows us to encode e.g. PLC with substantial annotations
(like position information) in situation where the stability is *not* critical, such as
for testing.
-}

{- Note [Encoding/decoding constructor tags using Flat]
Flat saves space when compared to CBOR by allowing tags to use the minimum
number of bits required for their encoding.

This requires specialised encode/decode functions for each constructor
that encodes a different number of possibilities. Here is a list of the
tags and their used/available encoding possibilities.

| Data type       | Function          | Used | Available |
|-----------------|-------------------|------|-----------|
| Terms           | encodeTerm        | 8    | 16        |

For format stability we are manually assigning the tag values to the
constructors (and we do not use a generic algorithm that may change this order).

All encodings use the function `safeEncodeBits :: NumBits -> Word8 -> Encoding`, which encodes
at most 8 bits of data, and the first argument specifies how many bits from the 8
available are actually used. This function also checks the size of the `Word8`
argument at runtime.

Flat uses an extra function in its class definition called `size`. Since we want
to reserve some space for future data constructors and we don't want to have the
sizes desynchronised from the encoding and decoding functions we have manual
implementations for them (if they have any constructors reserved for future use).

By default, Flat does not use any space to serialise `()`.
-}

-- | Using 4 bits to encode term tags.
termTagWidth :: NumBits
termTagWidth = 4

encodeTerm :: Word8 -> Encoding
encodeTerm = safeEncodeBits termTagWidth

decodeTerm :: Get Word8
decodeTerm = dBEBits8 termTagWidth

instance ( Closed uni
         , uni `Everywhere` Flat
         , Flat fun
         , Flat ann
         , Flat name
         , Flat (Binder name)
         ) => Flat (Term name uni fun ann) where
    encode = \case
        Var      ann n    -> encodeTerm 0 <> encode ann <> encode n
        Delay    ann t    -> encodeTerm 1 <> encode ann <> encode t
        LamAbs   ann n t  -> encodeTerm 2 <> encode ann <> encode (Binder n) <> encode t
        Apply    ann t t' -> encodeTerm 3 <> encode ann <> encode t <> encode t'
        Constant ann c    -> encodeTerm 4 <> encode ann <> encode c
        Force    ann t    -> encodeTerm 5 <> encode ann <> encode t
        Error    ann      -> encodeTerm 6 <> encode ann
        Builtin  ann bn   -> encodeTerm 7 <> encode ann <> encode bn

    decode = go =<< decodeTerm
        where go 0 = Var      <$> decode <*> decode
              go 1 = Delay    <$> decode <*> decode
              go 2 = LamAbs   <$> decode <*> (unBinder <$> decode) <*> decode
              go 3 = Apply    <$> decode <*> decode <*> decode
              go 4 = Constant <$> decode <*> decode
              go 5 = Force    <$> decode <*> decode
              go 6 = Error    <$> decode
              go 7 = Builtin  <$> decode <*> decode
              go _ = fail "Failed to decode Term Name ()"

    size tm sz = termTagWidth + sz + case tm of
        Var      ann n    -> getSize ann + getSize n
        Delay    ann t    -> getSize ann + getSize t
        LamAbs   ann n t  -> getSize ann + getSize n + getSize t
        Apply    ann t t' -> getSize ann + getSize t + getSize t'
        Constant ann c    -> getSize ann + getSize c
        Force    ann t    -> getSize ann + getSize t
        Error    ann      -> getSize ann
        Builtin  ann bn   -> getSize ann + getSize bn

instance ( Flat ann
         , Flat (Term name uni fun ann)
         ) => Flat (Program name uni fun ann) where
    encode (Program ann v t) = encode ann <> encode v <> encode t
    decode = Program <$> decode <*> decode <*> decode

instance ( Closed uni
         , uni `Everywhere` Flat
         , Flat fun
         ) => Flat (ETerm uni fun ) where
    encode = \case
        EVar      n    -> encodeTerm 0 <> encode n
        EDelay    t    -> encodeTerm 1 <> encode t
        ELamAbs   n t  -> encodeTerm 2 <> encode n <> encode t
        EApply    t t' -> encodeTerm 3 <> encode t <> encode t'
        EConstant c    -> encodeTerm 4 <> encode c
        EForce    t    -> encodeTerm 5 <> encode t
        EError         -> encodeTerm 6
        EBuiltin  bn   -> encodeTerm 7 <> encode bn

    decode = go =<< decodeTerm
        where go 0 = EVar      <$> decode
              go 1 = EDelay    <$> decode
              go 2 = ELamAbs   <$> decode <*> decode
              go 3 = EApply    <$> decode <*> decode
              go 4 = EConstant <$> decode
              go 5 = EForce    <$> decode
              go 6 = pure EError
              go 7 = EBuiltin  <$> decode
              go _ = fail "Failed to decode ETerm"

    size tm sz = termTagWidth + sz + case tm of
        EVar      n    -> getSize n
        EDelay    t    -> getSize t
        ELamAbs   n t  -> getSize n + getSize t
        EApply    t t' -> getSize t + getSize t'
        EConstant c    -> getSize c
        EForce    t    -> getSize t
        EError         -> 0
        EBuiltin  bn   -> getSize bn

instance (Flat (ETerm uni fun )
         ) => Flat (EProgram uni fun) where
    encode (EProgram v t) = encode v <> encode t
    decode = EProgram <$> decode <*> decode
